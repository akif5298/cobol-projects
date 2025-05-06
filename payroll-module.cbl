       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL-MODULE.
       AUTHOR.    Akif Rahman.
       DATE-WRITTEN. 2025-05-06.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO "EMPLOYEE.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT HOURS-FILE    ASSIGN TO "HOURS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PAYROLL-FILE  ASSIGN TO "PAYROLL.OUT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-FILE.
       01 EMPLOYEE-REC.
           05 EMP-ID     PIC 9(5).
           05 EMP-NAME   PIC A(30).
           05 EMP-RATE   PIC 9(3)V99.

       FD HOURS-FILE.
       01 HOURS-REC.
           05 HR-EMP-ID  PIC 9(5).
           05 HR-HOURS   PIC 9(3)V99.

       FD PAYROLL-FILE.
       01 PAYROLL-REC.
           05 P-EMP-ID   PIC 9(5).
           05 P-EMP-NAME PIC A(30).
           05 P-GROSS    PIC 9(6)V99.
           05 P-TAX      PIC 9(6)V99.
           05 P-NET      PIC 9(6)V99.

       WORKING-STORAGE SECTION.
       77 EOF-EMP      PIC X VALUE "N".
          88 END-OF-EMP           VALUE "Y".
          88 NOT-END-OF-EMP       VALUE "N".
       77 EOF-HRS      PIC X VALUE "N".
          88 END-OF-HRS           VALUE "Y".
          88 NOT-END-OF-HRS       VALUE "N".
       77 WS-HOURS     PIC 9(3)V99 VALUE 0.
       77 WS-RATE      PIC 9(3)V99 VALUE 0.
       77 WS-GROSS     PIC 9(6)V99 VALUE 0.
       77 WS-TAX-RATE  PIC V9(4)   VALUE .2000.
       77 WS-TAX       PIC 9(6)V99 VALUE 0.
       77 WS-NET       PIC 9(6)V99 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN INPUT EMPLOYEE-FILE
                INPUT HOURS-FILE
                OUTPUT PAYROLL-FILE
           PERFORM UNTIL END-OF-EMP
               READ EMPLOYEE-FILE
                   AT END
                       SET END-OF-EMP TO TRUE
                   NOT AT END
                       PERFORM PROCESS-EMP
               END-READ
           END-PERFORM
           CLOSE EMPLOYEE-FILE HOURS-FILE PAYROLL-FILE
           STOP RUN.

       PROCESS-EMP.
           MOVE 0          TO WS-HOURS WS-RATE WS-GROSS WS-TAX WS-NET

           CLOSE HOURS-FILE
           OPEN INPUT HOURS-FILE
           SET NOT-END-OF-HRS TO TRUE
           PERFORM UNTIL END-OF-HRS
               READ HOURS-FILE
                   AT END
                       SET END-OF-HRS TO TRUE
                   NOT AT END
                       IF HR-EMP-ID = EMP-ID
                           MOVE HR-HOURS TO WS-HOURS
                           MOVE EMP-RATE  TO WS-RATE
                           SET END-OF-HRS TO TRUE
                       END-IF
               END-READ
           END-PERFORM

           COMPUTE WS-GROSS = WS-HOURS * WS-RATE
           COMPUTE WS-TAX   = WS-GROSS * WS-TAX-RATE
           COMPUTE WS-NET   = WS-GROSS - WS-TAX

           MOVE EMP-ID    TO P-EMP-ID
           MOVE EMP-NAME  TO P-EMP-NAME
           MOVE WS-GROSS  TO P-GROSS
           MOVE WS-TAX    TO P-TAX
           MOVE WS-NET    TO P-NET

           WRITE PAYROLL-REC.

       END PROGRAM PAYROLL-MODULE.
