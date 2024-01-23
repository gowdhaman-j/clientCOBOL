:: Licensed Materials - Property of IBM
:: 5737-B16
:: © Copyright IBM Corp. 2023
:: US Government Users Restricted Rights- Use, duplication or disclosure 
:: restricted by GSA ADP  Schedule Contract with IBM Corp.
@echo off
setlocal enabledelayedexpansion

:: Get script's execution folder and remove the trailing backslash
set base=%~dp0
set base=%base:~0,-1%

:: Read configuration
echo Read configuration:
set configurationFile="%base%\conf.properties"
if not exist %configurationFile% (
	echo Error [1] Configuration file not found: '%configurationFile%'
	exit /B 1
)
for /F "tokens=1,2 delims=^=" %%G in ('findstr /c:"=" %configurationFile%') do (
	set %%G=%%H
	echo %%G=%%H
)

set ADDI_PROJECT_FOLDER=%ADDI_PROJECTS_FOLDER%\%ADDI_PROJECT_NAME%
echo ADDI_PROJECT_FOLDER=%ADDI_PROJECT_FOLDER%

set ADDI_INSTALL_FOLDER=%base%\..
set ADDI_BUILD_CLIENT=%ADDI_INSTALL_FOLDER%\IBM Application Discovery Build Client\Bin\Release\IBMApplicationDiscoveryBuildClient

set IMPORT_QUEUE_FILE="%base%\automation-import-queue.txt"
if not exist %IMPORT_QUEUE_FILE% (
	type NUL > %IMPORT_QUEUE_FILE%
)

:: Validate configuration
if not exist "%PROJECT_GITHUB_CLONE_FOLDER%" echo Error [6] GitHub project folder not found: "%PROJECT_GITHUB_CLONE_FOLDER%". && exit /B 6
if not exist "%ADDI_PROJECT_FOLDER%" echo  Error [6] Project folder not found: "%ADDI_PROJECT_FOLDER%". && exit /B 6

:: Get the latest DDL file, if any
if exist "%DDL_FOLDER%" (
	for /f "delims=" %%a in ('dir /b /OD "%DDL_FOLDER%\*.jcl" "%DDL_FOLDER%\*.ddl"') do set "ddlFile=%DDL_FOLDER%\%%a"
)
if not [%ddlFile%]==[] ( echo Using DDL file: %ddlFile% ) else ( echo No DDL file found. Skipping. )

:: Check environment
echo:
echo Checking if IBM Application Discovery Configuration Service is running
sc query IBMApplicationDiscoveryConfigurationService | find "RUNNING"
if errorlevel 1 (
	echo Error [9] IBM Application Discovery Configuration Service is not running.
	exit /b 9
)

::
:: Start the process
::

:: 1st step: Synchorize with your git repository
echo:
echo Synchronizing local folders with git (git pull)
call git -C %PROJECT_GITHUB_CLONE_FOLDER% pull
if %ERRORLEVEL% NEQ 0 (
	echo " Error [2] Aborting script execution."
	exit /B 2
)

:: 2nd step: Synchronize the ADDI project with the checked-out git repository
echo:
echo Synchronizing ADDI project with local folder (IBMApplicationDiscoveryBuildClient /umm1 %ADDI_PROJECT_NAME%)

if exist "%ADDI_PROJECT_FOLDER%\UpdateInBackgroundLog_*.txt" (
	for /f "delims=" %%a in ('dir /b /OD "%ADDI_PROJECT_FOLDER%\UpdateInBackgroundLog_*.txt"') do ( :: sort all project sync log files by asc date modified and take the newest
		set "previousProjectSyncLogFile=%ADDI_PROJECT_FOLDER%\%%a"
	)
)

call "%ADDI_BUILD_CLIENT%" /umm1 %ADDI_PROJECT_NAME%

if %ERRORLEVEL% NEQ 0 (
	echo " Error [7] ADDI project synchronization has failed."
	exit /B 7
)

if exist "%ADDI_PROJECT_FOLDER%\UpdateInBackgroundLog_*.txt" (
	for /f "delims=" %%a in ('dir /b /OD "%ADDI_PROJECT_FOLDER%\UpdateInBackgroundLog_*.txt"') do ( :: sort all project sync log files by asc date modified and take the newest
		set "projectSyncLogFile=%ADDI_PROJECT_FOLDER%\%%a"
	)
)

if ["%projectSyncLogFile%"]==["%previousProjectSyncLogFile%"] (		
	echo There is no new files to synchronize.
)

if ["%projectSyncLogFile%"]==[] (
	echo Project synchronization log: %projectSyncLogFile%
)

:: 3rd step: Incremental build of the ADDI project
echo:
echo Building ADDI project incrementally (IBMApplicationDiscoveryBuildClient /m1 %ADDI_PROJECT_NAME%)

if exist "%ADDI_PROJECT_FOLDER%\%ADDI_PROJECT_NAME%_*.txt" (
	for /f "delims=" %%a in ('dir /b /OD "%ADDI_PROJECT_FOLDER%\%ADDI_PROJECT_NAME%_*.txt"') do ( :: sort all build log files by asc date modified and take the newest
		set "previousBuildLogFile=%ADDI_PROJECT_FOLDER%\%%a"
	)
)

call "%ADDI_BUILD_CLIENT%" /m1 %ADDI_PROJECT_NAME%

if %ERRORLEVEL% NEQ 0 (
	echo " Error [11] ADDI incremental project build has failed."
	exit /B 11
)

:: Get the latest build log file and check if any new files have been build
if exist "%ADDI_PROJECT_FOLDER%\%ADDI_PROJECT_NAME%_*.txt" (
	for /f "delims=" %%a in ('dir /b /OD "%ADDI_PROJECT_FOLDER%\%ADDI_PROJECT_NAME%_*.txt"') do ( :: sort all build log files by asc date modified and take the newest
		set "buildLogFile=%ADDI_PROJECT_FOLDER%\%%a"
	)
)

if ["%buildLogFile%"]==[] (
	echo  Error [3] Cannot find project build log file in: '%ADDI_PROJECT_FOLDER%'
	exit /B 3
)

if ["%buildLogFile%"]==["%previousBuildLogFile%"] (
	echo  Error [10] Incremental build was interrupted or failed.
	exit /B 10
)

echo Project build log: %buildLogFile%

set programs=
for /F "tokens=2 delims=	" %%G in ('findstr /c:"zOS Cobol Program" "%buildLogFile%"') do ( :: for all built zOS COBOL Programs
	echo:
	echo Found an updated file: "%%G"
	set programName=
	
	for /F "delims=" %%a in ('where /R "%PROJECT_GITHUB_CLONE_FOLDER%" "%%G"') do ( :: resolve their full file path
		echo Resolved path to file: "%%a"
		for /F "tokens=2 delims=." %%p in ('findstr /i /c:"PROGRAM-ID." "%%a"') do ( :: read the line with PROGRAM-ID. and get the program name value only
			for /F "tokens=1* delims=' " %%t in ("%%p") do ( :: trim the program name value from whitespaces, single quotes and double quotes
				set programName=%%t
				:: trim program name from whitespaces
				set programName=!programName:"=!

				:: add the program to the import queue, if it hasn't been added already
				set importProgram=%%a^>!programName!
				findstr /c:"!importProgram!" %IMPORT_QUEUE_FILE%
				if !ERRORLEVEL! NEQ 0 (
					echo Adding program "!programName!" to import queue
					echo !importProgram!>>%IMPORT_QUEUE_FILE%
				)
			)
		)
	)
	
	if not ["!programName!"]==[""] ( set programs=!programs!!programName! ) else ( echo No program name found. Skipping. ) 
)

if ["%programs%"]==[""] echo None of the programs have been updated.

:: 4th step: Run the process to populate additional metadata for WCA4Z from ADDI and DDL
echo:
echo Populating additional metadata for WCA4Z from ADDI and DDL

set logFile="%base%\runControlFlowGenerator.log"
if exist %logFile% del %logFile%

for /F "usebackq tokens=1,2 delims=>" %%A in (%IMPORT_QUEUE_FILE%) do (
	echo:
	set programFile=%%A
	set programName=%%B
	
	echo Populating metadata for program !programName!
	if exist "!programFile!" (
		call "%base%\runControlFlowGenerator.bat" projectName=%ADDI_PROJECT_NAME% programName=!programName! ddlFile="%ddlFile%"

		if !ERRORLEVEL! EQU 0 (
			echo Done.
		) else (
			echo Error [8] The metadata generation process ran into an error [!ERRORLEVEL!].
			exit /b 8
		)
	) else (
		echo "!programFile!" has been deleted. Skipping...
	)
	
	call :removeProgramFileFromTheImportQueue "!programFile!"
)

exit /b 0

:removeProgramFileFromTheImportQueue

findstr /v /c:%1 %IMPORT_QUEUE_FILE% > %IMPORT_QUEUE_FILE%.tmp
move /y %IMPORT_QUEUE_FILE%.tmp %IMPORT_QUEUE_FILE% > nul
exit /b 0