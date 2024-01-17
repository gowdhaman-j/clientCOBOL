      * Licensed Materials - Property of IBM
      *
      * (c) Copyright IBM Corp. 2015,2016.
      *
      * US Government Users Restricted Rights - Use, duplication or
      * disclosure restricted by GSA ADP Schedule Contract
      * with IBM Corp.
       01  BNK1CHI.
           02  FILLER PIC X(12).
           02  COMPANYL    COMP  PIC  S9(4).
           02  COMPANYF    PICTURE X.
           02  FILLER REDEFINES COMPANYF.
             03 COMPANYA    PICTURE X.
           02  FILLER   PICTURE X(6).
           02  COMPANYI  PIC X(59).
           02  TACCNOL    COMP  PIC  S9(4).
           02  TACCNOF    PICTURE X.
           02  FILLER REDEFINES TACCNOF.
             03 TACCNOA    PICTURE X.
           02  FILLER   PICTURE X(6).
           02  TACCNOI  PIC X(8).
           02  FSCODE1L    COMP  PIC  S9(4).
           02  FSCODE1F    PICTURE X.
           02  FILLER REDEFINES FSCODE1F.
             03 FSCODE1A    PICTURE X.
           02  FILLER   PICTURE X(6).
           02  FSCODE1I  PIC X(6).
           02  FACNO1L    COMP  PIC  S9(4).
           02  FACNO1F    PICTURE X.
           02  FILLER REDEFINES FACNO1F.
             03 FACNO1A    PICTURE X.
           02  FILLER   PICTURE X(6).
           02  FACNO1I  PIC X(8).
           02  AMTL    COMP  PIC  S9(4).
           02  AMTF    PICTURE X.
           02  FILLER REDEFINES AMTF.
             03 AMTA    PICTURE X.
           02  FILLER   PICTURE X(6).
           02  AMTI  PIC X(13).
           02  TACCNO2L    COMP  PIC  S9(4).
           02  TACCNO2F    PICTURE X.
           02  FILLER REDEFINES TACCNO2F.
             03 TACCNO2A    PICTURE X.
           02  FILLER   PICTURE X(6).
           02  TACCNO2I  PIC X(8).
           02  TSORTCL    COMP  PIC  S9(4).
           02  TSORTCF    PICTURE X.
           02  FILLER REDEFINES TSORTCF.
             03 TSORTCA    PICTURE X.
           02  FILLER   PICTURE X(6).
           02  TSORTCI  PIC X(6).
           02  TACTBALL    COMP  PIC  S9(4).
           02  TACTBALF    PICTURE X.
           02  FILLER REDEFINES TACTBALF.
             03 TACTBALA    PICTURE X.
           02  FILLER   PICTURE X(6).
           02  TACTBALI  PIC X(14).
           02  TAVBALL    COMP  PIC  S9(4).
           02  TAVBALF    PICTURE X.
           02  FILLER REDEFINES TAVBALF.
             03 TAVBALA    PICTURE X.
           02  FILLER   PICTURE X(6).
           02  TAVBALI  PIC X(14).
           02  MESSAGEL    COMP  PIC  S9(4).
           02  MESSAGEF    PICTURE X.
           02  FILLER REDEFINES MESSAGEF.
             03 MESSAGEA    PICTURE X.
           02  FILLER   PICTURE X(6).
           02  MESSAGEI  PIC X(79).
           02  DUMMYL    COMP  PIC  S9(4).
           02  DUMMYF    PICTURE X.
           02  FILLER REDEFINES DUMMYF.
             03 DUMMYA    PICTURE X.
           02  FILLER   PICTURE X(6).
           02  DUMMYI  PIC X(1).
       01  BNK1CHO REDEFINES BNK1CHI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  COMPANYC    PICTURE X.
           02  COMPANYP    PICTURE X.
           02  COMPANYH    PICTURE X.
           02  COMPANYV    PICTURE X.
           02  COMPANYU    PICTURE X.
           02  COMPANYM    PICTURE X.
           02  COMPANYO  PIC X(59).
           02  FILLER PICTURE X(3).
           02  TACCNOC    PICTURE X.
           02  TACCNOP    PICTURE X.
           02  TACCNOH    PICTURE X.
           02  TACCNOV    PICTURE X.
           02  TACCNOU    PICTURE X.
           02  TACCNOM    PICTURE X.
           02  TACCNOO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  FSCODE1C    PICTURE X.
           02  FSCODE1P    PICTURE X.
           02  FSCODE1H    PICTURE X.
           02  FSCODE1V    PICTURE X.
           02  FSCODE1U    PICTURE X.
           02  FSCODE1M    PICTURE X.
           02  FSCODE1O  PIC X(6).
           02  FILLER PICTURE X(3).
           02  FACNO1C    PICTURE X.
           02  FACNO1P    PICTURE X.
           02  FACNO1H    PICTURE X.
           02  FACNO1V    PICTURE X.
           02  FACNO1U    PICTURE X.
           02  FACNO1M    PICTURE X.
           02  FACNO1O  PIC X(8).
           02  FILLER PICTURE X(3).
           02  AMTC    PICTURE X.
           02  AMTP    PICTURE X.
           02  AMTH    PICTURE X.
           02  AMTV    PICTURE X.
           02  AMTU    PICTURE X.
           02  AMTM    PICTURE X.
           02  AMTO  PIC X(13).
           02  FILLER PICTURE X(3).
           02  TACCNO2C    PICTURE X.
           02  TACCNO2P    PICTURE X.
           02  TACCNO2H    PICTURE X.
           02  TACCNO2V    PICTURE X.
           02  TACCNO2U    PICTURE X.
           02  TACCNO2M    PICTURE X.
           02  TACCNO2O  PIC X(8).
           02  FILLER PICTURE X(3).
           02  TSORTCC    PICTURE X.
           02  TSORTCP    PICTURE X.
           02  TSORTCH    PICTURE X.
           02  TSORTCV    PICTURE X.
           02  TSORTCU    PICTURE X.
           02  TSORTCM    PICTURE X.
           02  TSORTCO  PIC X(6).
           02  FILLER PICTURE X(3).
           02  TACTBALC    PICTURE X.
           02  TACTBALP    PICTURE X.
           02  TACTBALH    PICTURE X.
           02  TACTBALV    PICTURE X.
           02  TACTBALU    PICTURE X.
           02  TACTBALM    PICTURE X.
           02  TACTBALO PIC -9999999999.99.
           02  FILLER PICTURE X(3).
           02  TAVBALC    PICTURE X.
           02  TAVBALP    PICTURE X.
           02  TAVBALH    PICTURE X.
           02  TAVBALV    PICTURE X.
           02  TAVBALU    PICTURE X.
           02  TAVBALM    PICTURE X.
           02  TAVBALO PIC -9999999999.99.
           02  FILLER PICTURE X(3).
           02  MESSAGEC    PICTURE X.
           02  MESSAGEP    PICTURE X.
           02  MESSAGEH    PICTURE X.
           02  MESSAGEV    PICTURE X.
           02  MESSAGEU    PICTURE X.
           02  MESSAGEM    PICTURE X.
           02  MESSAGEO  PIC X(79).
           02  FILLER PICTURE X(3).
           02  DUMMYC    PICTURE X.
           02  DUMMYP    PICTURE X.
           02  DUMMYH    PICTURE X.
           02  DUMMYV    PICTURE X.
           02  DUMMYU    PICTURE X.
           02  DUMMYM    PICTURE X.
           02  DUMMYO  PIC X(1).
