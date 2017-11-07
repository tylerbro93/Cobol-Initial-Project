      ******************************************************************
      * Author: Tyler Brown
      * Date: 9/13/2017
      * Purpose: Create an invetory report from PR1FA17.txt
      * Tectonics: cobc -xo PROJECT1.exe --std=mf  PROJECT1.cbl
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJECT1.
       AUTHOR. TYLERBRO93.


       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. OCEANBLUE.
       OBJECT-COMPUTER. OCEANBLUE.


       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT SOURCE-FILE ASSIGN TO 'PR1FA17.txt'
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT REPORT-FILE ASSIGN TO 'report.txt'
           .


       DATA DIVISION.
       FILE SECTION.
       FD SOURCE-FILE.

       01 INVENTORY-RECORD.
           05 INVENTORY-ITEM                     .
              10 CATALOG-NUM                     PIC X(5).
              10 DESCRIPTION                     PIC X(20).
              10 UNIT-PURCHASE-PRICE             PIC 999V99.
              10 FILLER                          PIC X(7).
              10 QUANITY-ON-HAND                 PIC 9(5).
              10 QUANITY-ON-ORDER                PIC 9(5).
              10 REORDER-POINT                   PIC 9(5).
              10 RECIEVED-WEEKLY                 PIC 9(4).
              10 SOLD-WEEKLY                     PIC 9(4).
              10 RETURNED-WEEKLY                 PIC 9(4).

       FD REPORT-FILE.

       01 REPORT-RECORD                          PIC X(79).

       WORKING-STORAGE SECTION.

       01  WS-WORK-AREAS.
           05 EOF-FLAG                   PIC X(3) VALUE 'YES'.
           05 PROPER-SPACING             PIC 9(2) VALUE 0.
           05 LINE-NUM                   PIC 9(2) VALUE 10.

       01  WS-DATE.
           05 WS-MONTH    PIC 9(2).
           05 WS-DAY      PIC 9(2).
           05 WS-YEAR     PIC 9(2).

       01  HEADING-LINE1.
           05 HL-MONTH    PIC 9(2).
           05             PIC X VALUE '/'.
           05 HL-DAY      PIC 9(2).
           05             PIC X(3) VALUE '/20'.
           05 HL-YEAR     PIC 9(2).
           05             PIC X(5) VALUE SPACES.
           05             PIC X(3) VALUE 'TSB'.
           05             PIC X(10) VALUE SPACES.
           05             PIC X(27) VALUE 'Drakea Cart Parts Warehouse'.
           05             PIC X(16) VALUE SPACES.
           05             PIC X(5) VALUE 'PAGE '.
           05 HL-PAGE-NUM PIC 9(2).

       01  HEADING-LINE2.
           05             PIC X(36).
           05             PIC X(12) VALUE 'STOCK REPORT'.

       01  HEADING-LINE3.
           05                    PIC X(4) VALUE ' CAT'.
           05                    PIC X(11) VALUE '  '.
           05                    PIC X(4) VALUE 'ITEM'.
           05                    PIC X(11) VALUE '  '.
           05                    PIC X(8) VALUE 'PURCHASE'.
           05                    PIC X(3) VALUE '  '.
           05                    PIC X(8) VALUE 'QUANTITY'.
           05                    PIC X(4) VALUE '  '.
           05                    PIC X(8) VALUE 'QUANTITY'.
           05                    PIC X(4) VALUE '  '.
           05                    PIC X(7) VALUE 'REORDER'.

       01  HEADING-LINE4.
           05                    PIC X(4) VALUE ' NUM'.
           05                    PIC X(8) VALUE '  '.
           05                    PIC X(11) VALUE 'DESCRIPTION'.
           05                    PIC X(8) VALUE '  '.
           05                    PIC X(5) VALUE 'PRICE'.
           05                    PIC X(6) VALUE '  '.
           05                    PIC X(6) VALUE 'IN STK'.
           05                    PIC X(5) VALUE '  '.
           05                    PIC X(8) VALUE 'ON ORDER'.
           05                    PIC X(5) VALUE '  '.
           05                    PIC X(5) VALUE 'POINT'.

       01  DETAIL-LINE.
           05 DL-CATALOG-NUM-OUT PIC X(5).
           05                    PIC X(3) VALUE SPACES.
           05 DL-DESCRIPTION-OUT PIC X(20) VALUE SPACES.
           05                    PIC X(3) VALUE SPACES.
           05                    PIC X(1) VALUE '$'.
           05 DL-UPP-OUT         PIC 999.99.
           05                    PIC X(4) VALUE SPACES.
           05 DL-STOCK-OUT       PIC 999.99.
           05                    PIC X(6) VALUE SPACES.
           05 DL-ORDERED-OUT     PIC 999.99.
           05                    PIC X(5) VALUE SPACES.
           05 DL-REORDER-OUT     PIC 999.99.

       PROCEDURE DIVISION.

       100-MAIN-MODULE.
           PERFORM 125-HOUSEKEEPING
           PERFORM 150-READ-SOURCE-FILE
           PERFORM 250-CLOSE-ROUTINE
           .

       125-HOUSEKEEPING.
           MOVE 1 TO HL-PAGE-NUM
           OPEN INPUT SOURCE-FILE
           OUTPUT REPORT-FILE
           PERFORM 130-DATE-ROUTINE
           PERFORM 145-HEADING-ROUTINE
           .

       130-DATE-ROUTINE.
           ACCEPT WS-DATE FROM DATE
           MOVE WS-MONTH TO HL-MONTH
           MOVE WS-DAY TO HL-DAY
           MOVE WS-YEAR TO HL-YEAR
           .

       145-HEADING-ROUTINE.
           MOVE 1 TO PROPER-SPACING
           WRITE REPORT-RECORD FROM HEADING-LINE1
             AFTER ADVANCING PROPER-SPACING
           MOVE 3 TO PROPER-SPACING
           WRITE REPORT-RECORD FROM HEADING-LINE2
             AFTER ADVANCING PROPER-SPACING
           MOVE 2 TO PROPER-SPACING
           WRITE REPORT-RECORD FROM HEADING-LINE3
             AFTER ADVANCING PROPER-SPACING
           MOVE 1 TO PROPER-SPACING
           WRITE REPORT-RECORD FROM HEADING-LINE4
             AFTER ADVANCING PROPER-SPACING
           MOVE SPACES TO REPORT-RECORD
           WRITE REPORT-RECORD
             AFTER ADVANCING PROPER-SPACING
           .

       150-READ-SOURCE-FILE.
           PERFORM UNTIL EOF-FLAG = 'NO'
             READ SOURCE-FILE
               AT END
                  MOVE 'NO' TO EOF-FLAG
               NOT AT END
                  PERFORM 200-CONSTRUCT-DATA
                  ADD 1 TO LINE-NUM
                  IF LINE-NUM = 55
                     PERFORM 225-NEW-PAGE
                     MOVE 10 TO LINE-NUM
                  END-IF
             END-READ
           END-PERFORM
           .

       200-CONSTRUCT-DATA.
           MOVE CATALOG-NUM TO DL-CATALOG-NUM-OUT
           MOVE DESCRIPTION TO DL-DESCRIPTION-OUT
           MOVE UNIT-PURCHASE-PRICE TO DL-UPP-OUT
           MOVE QUANITY-ON-HAND TO DL-STOCK-OUT
           MOVE QUANITY-ON-ORDER TO DL-ORDERED-OUT
           MOVE REORDER-POINT TO DL-REORDER-OUT
           MOVE 1 TO PROPER-SPACING
           MOVE DETAIL-LINE TO REPORT-RECORD
           WRITE REPORT-RECORD
             AFTER ADVANCING PROPER-SPACING
           .

       225-NEW-PAGE.
           ADD 1 TO HL-PAGE-NUM
           MOVE SPACES TO REPORT-RECORD
           WRITE REPORT-RECORD
             AFTER ADVANCING PAGE
           PERFORM 145-HEADING-ROUTINE
           .

       250-CLOSE-ROUTINE.
           CLOSE SOURCE-FILE
                 REPORT-FILE
           STOP RUN
           .
