:: Licensed Materials - Property of IBM
:: 5737-B16
:: © Copyright IBM Corp. 2023
:: US Government Users Restricted Rights- Use, duplication or disclosure 
:: restricted by GSA ADP  Schedule Contract with IBM Corp.
@echo off
setlocal

:: Parse input arguments
set allInputArgs=%*

:parseInputArgsLoop
for /f "tokens=1*" %%a in ("%allInputArgs%") do (
	for /F "delims=^= tokens=1,2" %%i in ("%%a") do (
		if [%%i]==[projectName] set projectName=%%j
		if [%%i]==[programName] set programName=%%j
		if [%%i]==[ddlFile] set ddlFile=%%j
	)
   
	set allInputArgs=%%b
)
if defined allInputArgs goto :parseInputArgsLoop

if [%projectName%]==[] call :missingInputArgument projectName && exit /B 1
if [%programName%]==[] call :missingInputArgument programName && exit /B 1

:: Get script's execution folder and remove the trailing backslash
set base=%~dp0
set base=%base:~0,-1%

:: Get Batch Server's server.properties
set serverPropertiesFile="%base%\..\IBM Application Discovery Batch Server\conf\server.properties"
if not exist %serverPropertiesFile% (
	echo Configuration file not found: '%serverPropertiesFile%'
	exit /B 6
)

:: Read config
for /F "delims=^= tokens=2*" %%i in ('findstr /c:"ccs.server.host=" %serverPropertiesFile%') do (
	set "ccsServerHost=%%i"
)
for /F "delims=^= tokens=2*" %%i in ('findstr /c:"ccs.server.port=" %serverPropertiesFile%') do (
	set ccsServerPort=%%i
)
for /F "delims=^= tokens=2*" %%i in ('findstr /c:"ccs.environment=" %serverPropertiesFile%') do (
	set ENVIRONMENT_ID=%%i
)

if [%ccsServerHost%]==[] echo ccs.server.host is empty && exit /B 2
if [%ccsServerPort%]==[] echo ccs.server.port is empty && exit /B 3
if [%ENVIRONMENT_ID%]==[] echo ccs.environment is empty && exit /B 4

set ZOOKEEPER_HOST_PORT=%ccsServerHost%:%ccsServerPort%

:: Get SD ad-core-server file path
for /f "delims=" %%a in ('dir "%base%\ad-core-server-*.jar" /s /b') do set "adCoreServerFile="%%a""
if [%adCoreServerFile%]==[] (
	echo ad-core-server file not found in: '%base%\'
	exit /B 5
)

:: Set file path and clean up previous execution files

set logFile="%base%\%~n0.log"

set primaryKeysFile="%base%\PrimaryKeys.csv"
if exist %primaryKeysFile% del %primaryKeysFile%
if exist %primaryKeysFile% echo %primaryKeysFile% cannot be deleted. Check %logFile% for more details. Terminating... && exit /B 8

set foreignKeysFile="%base%\ForeignKeys.csv"
if exist %foreignKeysFile% del %foreignKeysFile%
if exist %foreignKeysFile% echo %foreignKeysFile% cannot be deleted. Check %logFile% for more details. Terminating... && exit /B 8

:: Generate PrimaryKeys.csv and ForeignKeys.csv
echo Generating PrimaryKeys.csv and ForeignKeys.csv

if exist "%ddlFile%" (
	java -jar DDL2CSV.jar "%ddlFile%" >> %logFile% 2>&1

	if not exist %primaryKeysFile% echo %primaryKeysFile% has not been created. Check %logFile% for more details. Terminating... && exit /B 7
	if not exist %foreignKeysFile% echo %foreignKeysFile% has not been created. Check %logFile% for more details. Terminating... && exit /B 7
) else (
	echo DDL file does not exist: "%ddlFile%". Skipping to next step.
)

:: Run the control flow generator and dump its output to a log file
echo Importing data into the database
java -cp %adCoreServerFile% -Dloader.main=com.ibm.ad.ADCoreServerCmdApplication org.springframework.boot.loader.PropertiesLauncher %projectName% %programName% >> %logFile% 2>&1

set importErrors=0
for /F "delims=] tokens=*" %%i in ('findstr /c:" ERROR " %logFile%') do (
	echo %%i
	set /A importErrors=importErrors + 1
)

if not %importErrors%==0 echo Errors occurred  during execution! Check %logFile% for more details. && exit /B 1000

exit /B 0


:missingInputArgument
echo %~1 is missing from the input arguments. Script usage "runControlFlowGenerator.bat projectName=<PROJECT_NAME> programName=<PROGRAM_NAME> ddlFile=<PATH_TO_A_DDL_OR_JCL_FILE>" && exit /B 0