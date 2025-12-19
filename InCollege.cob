       IDENTIFICATION DIVISION.
       PROGRAM-ID. INCOLLEGE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'InCollege-Input.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT OUTPUT-FILE ASSIGN TO 'InCollege-Output.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ACCOUNTS-FILE ASSIGN TO 'Accounts.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ACCOUNTS-STATUS.

           SELECT PROFILES-FILE ASSIGN TO 'Profiles.txt'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD key IS PROF-USERNAME
               ALTERNATE RECORD key IS PROF-FULLNAME WITH DUPLICATES
               FILE STATUS IS WS-PROFILES-STATUS.

           SELECT REQUESTS-FILE ASSIGN TO 'PendingRequests.txt'
                ORGANIZATION IS LINE SEQUENTIAL
                FILE STATUS IS WS-REQUESTS-STATUS.

           SELECT TEMP-REQUESTS-FILE
               ASSIGN TO "TempPending.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TEMP-REQUESTS-STATUS.

           SELECT CONNECTIONS-FILE ASSIGN TO 'Connections.txt'
                ORGANIZATION IS LINE SEQUENTIAL
                FILE STATUS IS WS-CONNECTIONS-STATUS.

           SELECT JOBS-FILE ASSIGN TO 'Jobs.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-JOBS-STATUS.

           SELECT APPLICATIONS-FILE ASSIGN TO 'Applications.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-APPLICATIONS-STATUS.

           SELECT APPLICATIONS-REPORT-FILE
               ASSIGN TO 'ApplicationsReport.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-APPLICATIONS-REPORT-STATUS.

           SELECT MESSAGES-FILE ASSIGN TO 'Messages.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-MESSAGES-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.  *> file descriptor for input file
       01 INPUT-RECORD PIC X(201). *> input record holds up to 201 chars
       *> Input record is 201 to detect inputs larger than 200 characters
       FD OUTPUT-FILE. *> file descriptor for output file
       01 OUTPUT-RECORD PIC X(220). *> output holds up to 200 chars
       *> Output record needs extra above Aboutme limit (200) for the
       *> "About me : " text itself (and possibly similar otherwise)
       FD ACCOUNTS-FILE.  *> file descriptor for accounts file
       01 USER-ACC. *> structure for each user record
           05 USERNAME PIC X(20).  *> username has up to 20 chars
           05 PASSW    PIC X(20).  *> password holds up to 20 chars
       FD PROFILES-FILE.
       01 PROFILE-REC. *> struct for profile records
           05 PROF-USERNAME    PIC X(20).  *> username, same as accounts
           05 PROF-FIRSTNAME   PIC X(50).
           05 PROF-LASTNAME    PIC X(50).
           *> FULLNAME is alternate key for profile
           05 PROF-FULLNAME    PIC X(100).   *> size must sum first+last
           05 PROF-UNIVERSITY  PIC X(100).
           05 PROF-MAJOR       PIC X(50).
           05 PROF-GRADYEAR    PIC X(4). *> note: X not 9, needs convert
           05 PROF-ABOUTME     PIC X(200). *> optional
           05 PROF-EXPERIENCE. *> optional
               10 PROF-EXP-COUNT PIC 9.
               10 PROF-EXP-ENTRY OCCURS 3 TIMES.
                   15  PROF-EXP-TITLE        PIC X(100).
                   15  PROF-EXP-COMPANY      PIC X(100).
                   15  PROF-EXP-DATES        PIC X(50).
                   15  PROF-EXP-DESCRIPTION  PIC X(200). *> optional
           05 PROF-EDUCATION. *> optional
               10 PROF-EDU-COUNT PIC 9.
               10 PROF-EDU-ENTRY OCCURS 3 TIMES.
                   15  PROF-EDU-DEGREE       PIC X(100).
                   15  PROF-EDU-UNIVERSITY   PIC X(100).
                   15  PROF-EDU-YEARS        PIC X(50).

       FD REQUESTS-FILE. *> file descriptor for requests file
       01 REQUEST-REC. *> struct for each request record
              05 REQ-SENDER    PIC X(20).
              05 REQ-RECIPIENT PIC X(20).
       FD CONNECTIONS-FILE. *> file descriptor for connections file
       01 CONNECTION-REC. *> struct for each connection record
              05 CONN-USERA PIC X(20).
              05 CONN-USERB PIC X(20).

       FD  TEMP-REQUESTS-FILE.
       01  TEMP-REQUESTS-RECORD.
           05  TEMP-REQ-SENDER      PIC X(20).
           05  TEMP-REQ-RECIPIENT   PIC X(20).

       FD JOBS-FILE.
       01 JOB-RECORD.
           05 JOB-POSTER      PIC X(20). *> username of job poster
           05 JOB-TITLE       PIC X(100).
           05 JOB-DESCRIPTION PIC X(200).
           05 JOB-EMPLOYER    PIC X(100).
           05 JOB-LOCATION    PIC X(100).
           05 JOB-SALARY      PIC X(50).

       FD APPLICATIONS-FILE.
       01 APPLICATION-RECORD.
            05 APP-USERNAME    PIC X(20).
            05 APP-JOB-TITLE   PIC X(100).
            05 APP-EMPLOYER    PIC X(100).
            05 APP-LOCATION    PIC X(100).
            88 WSEOF VALUE HIGH-VALUE.

       FD APPLICATIONS-REPORT-FILE.
       01 APPLICATION-REPORT-RECORD PIC X(320).

       FD MESSAGES-FILE.
       01 MESSAGE-RECORD.
           05 MSG-SENDER      PIC X(20).
           05 MSG-RECEIVER   PIC X(20).
           05 MSG-CONTENT     PIC X(200).
           05 MSG-TIMESTAMP   PIC X(21).
           *> timestamp format: YYMMDD HHMMSS


       WORKING-STORAGE SECTION.    *> defines temporary variables
       01 WS-USER-COUNT PIC 9 VALUE 0. *> counts num of users made
       01 WS-MAX-USERS PIC 9 VALUE 5.  *> sets max users to 5
       01 WS-USERNAME PIC X(20).   *> temp storage for user input
       01 WS-PASSW PIC X(20).   *> temp storage for pass input
       01 WS-VALID-PASSW PIC X VALUE 'N'.   *> flag for verification
       01 WS-FOUND PIC X VALUE 'N'. *> flag for if user found
       01 WS-CHOICE PIC 9. *> login display menu choice (1 or 2)
       01 IDX PIC 9(02). *> index variable for loops
       01 JDX PIC 9(02). *> second index variable for nested loops
       01 CHAR PIC X.  *> character for password validation
       01 UPPER-COUNT PIC 9(02). *> count of uppercase in pass
       01 LOWER-COUNT PIC 9(02). *> count of lowercase in pass
       01 DIGIT-COUNT PIC 9(02). *> count of digits in pass
       01 SPECIAL-COUNT PIC 9(02). *> count of special chars in pass
       01 EOF PIC X VALUE 'N'. *> flag when EOF is reached
       01 WS-SKILL-CHOICE PIC 9 VALUE 0. *> skill menu choice (1-6)
       01 WS-MENU-CHOICE PIC 9 VALUE 0. *> post-login choice (1-3)
       01 INPUT-EOF-FLAG PIC X VALUE 'N'. *> flag for input file EOF
       01 WS-ACCOUNTS-STATUS PIC XX. *> file status for Accounts.txt
       01 WS-PROFILES-STATUS PIC XX. *> file status for Profiles.txt
       01 WS-PROFILE-FOUND PIC X VALUE 'N'.
       01 WS-LOGGED-USER PIC X(20). *> also used for profile key
       *> For LENGTH-OF-INPUT procedure
       01 WS-REQUESTS-STATUS PIC XX VALUE SPACES. *> file status for
       *> Requests
       01 WS-CONNECTIONS-STATUS PIC XX VALUE SPACES.
       *> file status for connections

       01 WS-TARGET-USER PIC X(20) VALUE SPACES.
       *> user to send req to user
       01 WS-TARGET-FULLNAME PIC X(100) VALUE SPACES.
       *> full name of target

       01 WS-REQUEST-FOUND PIC X VALUE 'N'. *> flag for request found
       01 WS-CONNECTION-FOUND PIC X VALUE 'N'.
       *> flag for connection found

       01 WS-TRAILING-SPACES PIC 9(3) VALUE ZERO.
       *> For LENGTH-OF-INPUT procedure
       01 WS-INPUT-LENGTH PIC 9(3) VALUE ZERO.
       *> For LENGTH-OF-INPUT procedure
       01 WS-LENGTH-OF-INPUT-MAX PIC 9(3) VALUE ZERO.
       01 WS-READ-EOF PIC X. *> Flag for READ NEXT RECORD loops
       01 WS-COUNT PIC 9(3). *> profile count (possibly other counts)
       01 WS-SEARCH-FULLNAME PIC X(100). *>temp storage for search input

       01 WS-PROF-REC.
           05 WS-PROF-USERNAME PIC X(20). *> profile key
           05 WS-FIRSTNAME PIC X(50).
           05 WS-LASTNAME PIC X(50).
           *> FULLNAME is alternate key for profile
           05 WS-FULLNAME    PIC X(100).   *> size must sum first+last
           05 WS-UNIVERSITY PIC X(100).
           05 WS-MAJOR PIC X(50).
           05 WS-GRADYEAR PIC X(4).
           05 WS-ABOUTME PIC X(200). *> optional
           05 WS-EXPERIENCE. *> optional
               10 WS-EXP-COUNT PIC 9 VALUE 0.
               10 WS-EXP-ENTRIES OCCURS 3 TIMES.
                   15 WS-EXP-TITLE PIC X(100).
                   15 WS-EXP-COMPANY PIC X(100).
                   15 WS-EXP-DATES PIC X(50).
                   15 WS-EXP-DESCRIPTION PIC X(200). *> optional
            05 ES-EDUCATION. *> optional
               10 WS-EDU-COUNT PIC 9 VALUE 0.
               10 WS-EDU-ENTRIES OCCURS 3 TIMES.
                   15 WS-EDU-DEGREE PIC X(100).
                   15 WS-EDU-UNIVERSITY PIC X(100).
                   15 WS-EDU-YEARS PIC X(50).
       01 WS-EXP-IDX PIC 9.
       01 WS-EDU-IDX PIC 9.
       01  WS-DELETE-REQUEST           PIC X VALUE 'N'.
       01  WS-TEMP-REQUESTS-STATUS     PIC XX.
       01  WS-TEMP-SENDER              PIC X(20).
       01  WS-TEMP-RECIPIENT           PIC X(20).

       *> variables for jobs and applications functionality
       01 WS-APPLICATIONS-STATUS PIC XX.
       01 WS-APPLICATIONS-REPORT-STATUS PIC XX.
       01 WS-JOB-CHOICE PIC 999 VALUE 0.
       01 WS-JOB-COUNT PIC 999 VALUE 0.
       01 WS-APP-COUNT PIC 999 VALUE 0.
       01 WS-APP-COUNT-DISPLAY PIC ZZ9 VALUE 0.
       01 WS-APPLY-CHOICE PIC 999 VALUE 0.

       *> variables for job search functionality
       01 WS-JOBS-STATUS PIC XX VALUE SPACES.
       01 WS-JOB-TITLE PIC X(100) VALUE SPACES.
       01 WS-JOB-DESCRIPTION PIC X(200) VALUE SPACES.
       01 WS-JOB-EMPLOYER PIC X(100) VALUE SPACES.
       01 WS-JOB-LOCATION PIC X(100) VALUE SPACES.
       01 WS-JOB-SALARY PIC X(50) VALUE SPACES.

       *> Variables and filler for applications report
       01 AppR-PageHeading.
            05 FILLER PIC X(24) VALUE "Application Summary for".
            05 AppR-PrnUserName PIC X(20).
       01 AppR-PageFooting.
            05 FILLER PIC X(15) VALUE SPACES.
            05 FILLER PIC X(10) VALUE "Page: ".
            05 AppR-PrnPageNum PIC Z9.
       01 AppR-Headers.
           05 FILLER PIC X(50) VALUE "Job Title".
           05 FILLER PIC X(12) VALUE SPACES.
           05 FILLER PIC X(50) VALUE "Employer".
           05 FILLER PIC X(11) VALUE SPACES.
           05 FILLER PIC X(50) VALUE "Location".
       01 AppR-HorizontalLine.
           05 FILLER OCCURS 150 TIMES PIC X VALUE '-'.
       01 AppR-JobDetailLine.
           05 FILLER PIC X VALUE SPACES.
           05 AppR-PrnJobTitle PIC X(50).
           05 FILLER PIC X(11) VALUE SPACES.
           05 AppR-PrnEmployer PIC X(50).
           05 FILLER PIC X(11) VALUE SPACES.
           05 AppR-PrnLocation PIC X(50).
       01 AppR-TotalApplicationsLine.
           05 FILLER PIC X(20) VALUE "Total Applications: ".
           05 AppR-TotalApplicationsCount PIC ZZ9 VALUE 0.
       01 AppR-ReportFooting PIC X(13) VALUE "END OF REPORT".
       01 AppR-LineCount PIC 9(3) VALUE 0. *> In play but unused
           88 AppR-NewPageRequired VALUE 400 THRU 999.
       01 AppR-PageCount PIC 9 VALUE 1. *> In play but unused

       *> variables for Messages functionality
       01 WS-MESSAGES-STATUS PIC XX VALUE SPACES.
       01 WS-TARGET-USERNAME PIC X(20) VALUE SPACES.
       01 WS-DATE PIC X(8) VALUE SPACES.
       01 WS-TIME PIC X(6) VALUE SPACES.
       01 WS-MESSAGES-RECORD.
           05 WS-MSG-SENDER      PIC X(20).
           05 WS-MSG-RECEIVER   PIC X(20).
           05 WS-MSG-CONTENT     PIC X(200).
           05 WS-MSG-TIMESTAMP   PIC X(21).
           *> timestamp format: YYMMDD HHMMSS
       01 WS-FORMATTED-TIMESTAMP PIC X(16). *> YYYY-MM-DD HH:MM
       01 WS-TS-YEAR PIC X(2).
       01 WS-TS-MONTH PIC X(2).
       01 WS-TS-DAY PIC X(2).
       01 WS-TS-HOUR PIC X(2).
       01 WS-TS-MINUTE PIC X(2).
       01 WS-TS-FULL-YEAR PIC X(4).

       *> MAIN CODE ENTRY ----------------------------------------------

       *> main code process
       PROCEDURE DIVISION.
           PERFORM INITIALIZE-FILES
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE
           PERFORM COUNT-EXISTING-USERS
           PERFORM START-SCREEN
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       *> starts the procedure
       START-SCREEN.
           MOVE "-----------------------------------" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE "Welcome to InCollege!" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE "-----------------------------------" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE "1. Log In" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE "2. Create New Account" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE "Enter your choice:" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT

           PERFORM READ-SAFELY
           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF *> check if end

           MOVE INPUT-RECORD(1:1) TO WS-CHOICE *> get 1st char as choice

           EVALUATE WS-CHOICE
               WHEN 1 *> if 1 them log in logic
                   MOVE 'N' TO WS-FOUND *> set found flag to No
                   PERFORM UNTIL WS-FOUND = 'Y' OR INPUT-EOF-FLAG = 'Y'
                       PERFORM LOGIN
                   END-PERFORM
               WHEN 2 *> if 2 then create account logic
                   PERFORM REGISTRATION
               WHEN OTHER *> if anything else its invalid
                   MOVE "Invalid choice." TO OUTPUT-RECORD
                   PERFORM WRITE-OUTPUT
           END-EVALUATE.

       *> SYSTEM WIDE FUNCTIONS ----------------------------------------

       *> creates files if they do not exist
       INITIALIZE-FILES.
           OPEN INPUT ACCOUNTS-FILE
           IF WS-ACCOUNTS-STATUS = "35" *> file not found
               OPEN OUTPUT ACCOUNTS-FILE *> create empty file
               CLOSE ACCOUNTS-FILE
           ELSE
               CLOSE ACCOUNTS-FILE
               END-IF

           OPEN I-O PROFILES-FILE
           IF WS-PROFILES-STATUS = "35"
               OPEN OUTPUT PROFILES-FILE
               CLOSE PROFILES-FILE
           ELSE
               CLOSE PROFILES-FILE
           END-IF

           OPEN INPUT JOBS-FILE
           IF WS-JOBS-STATUS = "35"
               OPEN OUTPUT JOBS-FILE
               CLOSE JOBS-FILE
           ELSE
               CLOSE JOBS-FILE
           END-IF

           OPEN INPUT APPLICATIONS-FILE
           IF WS-APPLICATIONS-STATUS = "35"
               OPEN OUTPUT APPLICATIONS-FILE
               CLOSE APPLICATIONS-FILE
           ELSE
               CLOSE APPLICATIONS-FILE
           END-IF

           OPEN INPUT MESSAGES-FILE
           IF WS-MESSAGES-STATUS = "35"
               OPEN OUTPUT MESSAGES-FILE
               CLOSE MESSAGES-FILE
           ELSE
               CLOSE MESSAGES-FILE
           END-IF.

       *> count users in file at program startup
       COUNT-EXISTING-USERS.
           MOVE 0 TO WS-USER-COUNT
           MOVE 'N' TO EOF *> initialize EOF flag to No
           OPEN INPUT ACCOUNTS-FILE *> open Accounts for reading
           PERFORM UNTIL EOF = 'Y' *> read until EOF
               READ ACCOUNTS-FILE  *> read next account
                   AT END MOVE 'Y' TO EOF *> if EOF change flag to Yes
                   NOT AT END  *> if not at EOF
                       ADD 1 TO WS-USER-COUNT *> increment user counter
                   END-ADD
               END-READ
           END-PERFORM
           CLOSE ACCOUNTS-FILE.

       *> safe read with EOF handling
       READ-SAFELY.
           READ INPUT-FILE *> read next line
               AT END *> if we reach EOF
                   MOVE 'Y' TO INPUT-EOF-FLAG *> set flag to end
                   MOVE "End of input and program."
                       TO OUTPUT-RECORD
                   PERFORM WRITE-OUTPUT
               NOT AT END *> if we do not reach the end, continue
                   CONTINUE
           END-READ.

       *> writes output to screen and output file
       WRITE-OUTPUT.
           *> FUNCTION TRIM() is modern COBOL that trims spaces
           *> TRIM(data TRAILING) trims from back instead of front
           DISPLAY FUNCTION TRIM(OUTPUT-RECORD TRAILING)
           WRITE OUTPUT-RECORD.

       *> USER MANAGEMENT ----------------------------------------------

       *> creates a new account
       REGISTRATION.
           IF WS-USER-COUNT >= WS-MAX-USERS *> if max users is reached
               STRING "All permitted accounts have been created, "
                      "please come back later."
                   DELIMITED BY SIZE *> go through both strings
                   INTO OUTPUT-RECORD
               END-STRING
               PERFORM WRITE-OUTPUT
               EXIT PARAGRAPH *> exit early
           END-IF

           MOVE "Enter username:" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           PERFORM READ-SAFELY
           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF *> check if end
           MOVE INPUT-RECORD(1:20) TO WS-USERNAME  *> store user in var

           PERFORM CHECK-USER-EXISTS *> check if username exists
           IF WS-FOUND = 'Y' *> if username already exists
               MOVE "Username already exists." TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               CLOSE INPUT-FILE
               CLOSE OUTPUT-FILE
               STOP RUN *> exit program so user can restart
           END-IF

           MOVE "Enter password:" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           PERFORM READ-SAFELY
           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF *> check if end
           MOVE INPUT-RECORD(1:20) TO WS-PASSW *> store password in var

           PERFORM VALIDATE-PASSW *> call function to validate password
           IF WS-VALID-PASSW = 'Y' *> if password is valid
               OPEN EXTEND ACCOUNTS-FILE *> open Account for appending
               MOVE WS-USERNAME TO USERNAME *> copy username to file
               MOVE WS-PASSW TO PASSW *> Copy password to file
               WRITE USER-ACC *> write new user record to file
               END-WRITE
               CLOSE ACCOUNTS-FILE

               ADD 1 TO WS-USER-COUNT *> increment user counter
               END-ADD
               MOVE "Account created successfully." TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               PERFORM LOGIN
           ELSE *> if password is invalid
               MOVE "Invalid password." TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
           END-IF.

       *> check if username already exists
       CHECK-USER-EXISTS.
           MOVE 'N' TO WS-FOUND *> initialize found flag to No
           MOVE 'N' TO EOF *> initialize EOF flag to No
           OPEN INPUT ACCOUNTS-FILE *> open Accounts for reading
           PERFORM UNTIL EOF = 'Y' OR WS-FOUND = 'Y'  *> until EOF/found
               READ ACCOUNTS-FILE  *> read next account
                   AT END MOVE 'Y' TO EOF *> if EOF change flag to Yes
                   NOT AT END  *> if not at end of file
                       IF USERNAME = WS-USERNAME *> compare usernames
                           MOVE 'Y' TO WS-FOUND *> found flag to Yes
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ACCOUNTS-FILE.

       *> validates password according to criteria
       VALIDATE-PASSW.
           IF FUNCTION LENGTH(FUNCTION TRIM(WS-PASSW)) > 12
               MOVE "Password cannot exceed 12 characters."
               TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               MOVE 'N' TO WS-VALID-PASSW
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO UPPER-COUNT *> initialize uppercase count
           MOVE 0 TO LOWER-COUNT *> initialize lowercase count
           MOVE 0 TO DIGIT-COUNT *> initialize digit count
           MOVE 0 TO SPECIAL-COUNT *> initialize special char
           MOVE 'Y' TO WS-VALID-PASSW *> password initially valid

           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX >
           LENGTH OF WS-PASSW  *> loop through each char
               MOVE WS-PASSW(IDX:1) TO CHAR *> get char from password
               EVALUATE TRUE *> check char
                   WHEN CHAR >= 'A' AND CHAR <= 'Z'  *> if uppercase
                       ADD 1 TO UPPER-COUNT END-ADD *> add to counter
                   WHEN CHAR >= 'a' AND CHAR <= 'z' *> if lowercase
                       ADD 1 TO LOWER-COUNT END-ADD *> add to counter
                   WHEN CHAR >= '0' AND CHAR <= '9' *> if digit
                       ADD 1 TO DIGIT-COUNT END-ADD *> add to counter
                   WHEN CHAR = '!' OR CHAR = '@' OR CHAR = '#' OR
                        CHAR = '$' OR CHAR = '%' OR CHAR = '^' OR
                        CHAR = '&' OR CHAR = '*' *> if is special
                       ADD 1 TO SPECIAL-COUNT END-ADD *> add to counter
               END-EVALUATE
           END-PERFORM

           *> if not 8 chars without space
           IF FUNCTION LENGTH(FUNCTION TRIM(WS-PASSW)) < 8
               MOVE "Password needs at least 8 characters."
               TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               MOVE 'N' TO WS-VALID-PASSW *> mark as invalid
           END-IF

           IF UPPER-COUNT = 0 *> if no uppercase
               MOVE "Password needs at least one uppercase letter."
               TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               MOVE 'N' TO WS-VALID-PASSW *> mark as invalid
           END-IF

           IF LOWER-COUNT = 0 *> if no lowercase
               MOVE "Password needs at least one lowercase letter."
               TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               MOVE 'N' TO WS-VALID-PASSW *> mark as invalid
           END-IF

           IF DIGIT-COUNT = 0 *> if no digits
               MOVE "Password needs at least one digit."
               TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               MOVE 'N' TO WS-VALID-PASSW *> mark as invalid
           END-IF

           IF SPECIAL-COUNT = 0 *> if no special chars
               MOVE "Password needs at least one special character."
               TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               MOVE 'N' TO WS-VALID-PASSW *> mark as invalid
           END-IF.

       *> performs login procedure
       LOGIN.
           MOVE "Please enter your username:" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           PERFORM READ-SAFELY
           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF *> check if end
           MOVE INPUT-RECORD(1:20) TO WS-USERNAME *> store user in var

           MOVE "Please enter your password:" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           PERFORM READ-SAFELY
           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF *> check if end
           MOVE INPUT-RECORD(1:20) TO WS-PASSW *> store pass in variable

           PERFORM VERIFY-LOGIN *> call function to verify login

           IF WS-FOUND = 'Y' *> check if user was found
               MOVE "You have successfully logged in." TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               MOVE "-----------------------------------"
               TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT

               MOVE SPACES TO OUTPUT-RECORD
               STRING "Welcome, " DELIMITED BY SIZE *> welcome message
                      WS-USERNAME DELIMITED BY SPACE *> use username
                      "!" DELIMITED BY SIZE
                  INTO OUTPUT-RECORD
               END-STRING
               PERFORM WRITE-OUTPUT
               MOVE WS-USERNAME TO WS-LOGGED-USER
               PERFORM POST-LOGIN-MENU *> go to post-login function
           ELSE *> if user not found
               MOVE "Incorrect username/password, please try again."
                   TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
           END-IF.

       *> verifies login info against Accounts.txt
       VERIFY-LOGIN. *> verify login info with that in Accounts.txt
           MOVE 'N' TO WS-FOUND *> initialize found flag to No
           MOVE 'N' TO EOF *> initialize EOF flag to No
           OPEN INPUT ACCOUNTS-FILE *> open Accounts for reading
           PERFORM UNTIL EOF = 'Y' OR WS-FOUND = 'Y'  *> until EOF/found
               READ ACCOUNTS-FILE  *> read next acc
                   AT END MOVE 'Y' TO EOF *> if EOF change flag to Yes
                   NOT AT END  *> if entire file was read
                       IF USERNAME = WS-USERNAME AND
                          PASSW = WS-PASSW *> compare values
                           MOVE 'Y' TO WS-FOUND *> found flag to Yes
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE ACCOUNTS-FILE.

       *> procedure to log out and exit to start screen
       LOG-OUT-EXIT.
           MOVE "Logging you out to Start Screen." TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE SPACES TO OUTPUT-RECORD
           STRING "Goodbye, " DELIMITED BY SIZE
                      WS-USERNAME DELIMITED BY SPACE *> use username
                      "!" DELIMITED BY SIZE
                  INTO OUTPUT-RECORD
           END-STRING
           PERFORM WRITE-OUTPUT
           MOVE SPACES TO WS-LOGGED-USER
           PERFORM START-SCREEN.

       *> POST LOGIN MENU ----------------------------------------------

       *> displays post-login menu (all options)
       POST-LOGIN-MENU.
           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF *> check end
           MOVE "-----------------------------------" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE "0. Log Out" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE "1. Create/Edit My Profile" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE "2. View My Profile" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE "3. Search for a job" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE "4. Find someone you know" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE "5. Learn a New Skill" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE "6. View My Pending Connection Requests"
                TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE "7. View My Network" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE "8. Messages" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE "Enter your choice:" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT

           PERFORM READ-SAFELY
           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF *> check if end

           MOVE INPUT-RECORD(1:1) TO WS-MENU-CHOICE *> get first char
           EVALUATE WS-MENU-CHOICE
               WHEN 0
                   PERFORM LOG-OUT-EXIT
               WHEN 1
                   PERFORM CREATE-EDIT-PROFILE
                   PERFORM POST-LOGIN-MENU
               WHEN 2
                   PERFORM LOADANDVIEW-PROFILE
                   PERFORM POST-LOGIN-MENU
               WHEN 3
                   PERFORM JOB-SEARCH-MENU
                   PERFORM POST-LOGIN-MENU
               WHEN 4
                   PERFORM SEARCHANDVIEW-PROFILE
                   PERFORM POST-LOGIN-MENU
               WHEN 5
                   PERFORM LEARN-SKILL-MENU *> go to the skills menu
                   PERFORM POST-LOGIN-MENU
               WHEN 6
                   PERFORM VIEW-PENDING-REQUESTS
                   PERFORM POST-LOGIN-MENU
               WHEN 7
                   PERFORM VIEW-MY-NETWORK
                   PERFORM POST-LOGIN-MENU
               WHEN 8
                   PERFORM MESSAGES-MENU
                   PERFORM POST-LOGIN-MENU
               WHEN OTHER *> if user chose anything else
                   MOVE "Invalid choice." TO OUTPUT-RECORD
                   PERFORM WRITE-OUTPUT
                   PERFORM POST-LOGIN-MENU  *> go back to post-login
           END-EVALUATE.

       *> PROFILE MANAGEMENT -------------------------------------------

       *> procedure to create or edit user profile
       CREATE-EDIT-PROFILE.
           MOVE "------- Create/Edit Profile -------" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT

           INITIALIZE WS-PROF-REC *> Clear WS Profile for multiple edits
           MOVE WS-LOGGED-USER TO WS-PROF-USERNAME

           MOVE "Enter First Name:" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           PERFORM READ-SAFELY
           IF INPUT-RECORD = SPACES
               MOVE 'Y' TO INPUT-EOF-FLAG
               MOVE "First name is a required field. Try again."
                   TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
           END-IF
           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF
           MOVE INPUT-RECORD TO WS-FIRSTNAME

           MOVE "Enter Last Name:" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           PERFORM READ-SAFELY
           IF INPUT-RECORD = SPACES
               MOVE 'Y' TO INPUT-EOF-FLAG
               MOVE "Last name is a required field. Try again."
                   TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
           END-IF
           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF
           MOVE INPUT-RECORD TO WS-LASTNAME

           *> Populate FULLNAME from FIRSTNAME and LASTNAME
           STRING WS-FIRSTNAME DELIMITED BY SPACE
               " " DELIMITED BY SIZE
               WS-LASTNAME DELIMITED BY SPACE
               INTO WS-FULLNAME
           END-STRING

           MOVE "Enter University/College Attended:" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           PERFORM READ-SAFELY
           IF INPUT-RECORD = SPACES
               MOVE 'Y' TO INPUT-EOF-FLAG
               STRING "University/College Attended is a required field."
                      " Try again."
                      DELIMITED BY SIZE
                   INTO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
           END-IF
           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF
           MOVE INPUT-RECORD TO WS-UNIVERSITY

           MOVE "Enter Major:" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           PERFORM READ-SAFELY
           IF INPUT-RECORD = SPACES
               MOVE 'Y' TO INPUT-EOF-FLAG
               MOVE "Major is a required field. Try again."
                   TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
           END-IF
           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF
           MOVE INPUT-RECORD TO WS-MAJOR

           PERFORM WITH TEST AFTER UNTIL WS-VALID-PASSW = 'Y'
               MOVE "Enter Graduation Year (YYYY):" TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               PERFORM READ-SAFELY
               IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF
               MOVE INPUT-RECORD TO WS-GRADYEAR

               PERFORM VALIDATE-GRAD-YEAR
               *> Above uses WS-VALID-PASSW as validity
               IF WS-VALID-PASSW = 'N'
                   MOVE 'Y' TO INPUT-EOF-FLAG
                   EXIT PARAGRAPH
               END-IF
           END-PERFORM

           MOVE SPACES TO OUTPUT-RECORD
           STRING "Enter About Me (optional, max 200 chars, "
                  "enter blank line to skip):"
               DELIMITED BY SIZE INTO OUTPUT-RECORD
           END-STRING
           PERFORM WRITE-OUTPUT
           PERFORM READ-SAFELY

           MOVE 200 TO WS-LENGTH-OF-INPUT-MAX *> Set for LENGTH-OF-INPUT
           PERFORM INPUT-LENGTH-CHECK
           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF
           MOVE INPUT-RECORD TO WS-ABOUTME

           PERFORM GET-EXPERIENCE

           PERFORM GET-EDUCATION

           PERFORM SAVE-PROFILE

           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF
           MOVE "Profile saved successfully!" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT.

       *> procedure to validate graduation year
       VALIDATE-GRAD-YEAR.
           MOVE 'Y' TO WS-VALID-PASSW *> re-use of password validity
           IF FUNCTION LENGTH(FUNCTION TRIM(WS-GRADYEAR)) NOT = 4
               MOVE "Graduation year must be 4 digits." TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               MOVE 'N' TO WS-VALID-PASSW
               EXIT PARAGRAPH
           END-IF
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 4
               MOVE WS-GRADYEAR(IDX:1) TO CHAR
               IF CHAR < '0' OR CHAR > '9'
                   MOVE "Graduation year must be numeric."
                       TO OUTPUT-RECORD
                   PERFORM WRITE-OUTPUT
                   MOVE 'N' TO WS-VALID-PASSW
                   EXIT PARAGRAPH
               END-IF
           END-PERFORM
           IF WS-GRADYEAR < "1900" OR WS-GRADYEAR > "2100"
               MOVE "Graduation year must be between 1900 and 2100."
                   TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               MOVE 'N' TO WS-VALID-PASSW
           END-IF.

       *> procedure to get experience entries
       GET-EXPERIENCE.
           MOVE 0 TO WS-EXP-COUNT
           MOVE 1 TO WS-EXP-IDX
           PERFORM UNTIL WS-EXP-IDX > 3 OR INPUT-EOF-FLAG = 'Y'
               MOVE SPACES TO OUTPUT-RECORD
               STRING "Add Experience (optional, max 3 entries. Enter "
                      "'DONE' to finish, anything else to continue):"
                   DELIMITED BY SIZE INTO OUTPUT-RECORD
               END-STRING
               PERFORM WRITE-OUTPUT

               PERFORM READ-SAFELY
               IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF
               IF INPUT-RECORD = "DONE"
                   MOVE "DONE" TO OUTPUT-RECORD
                   PERFORM WRITE-OUTPUT
                   EXIT PERFORM
               END-IF

               MOVE SPACES TO OUTPUT-RECORD
               STRING "Experience #" WS-EXP-IDX " - Title:"
                   DELIMITED BY SIZE INTO OUTPUT-RECORD
               END-STRING
               PERFORM WRITE-OUTPUT
               PERFORM READ-SAFELY
               IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF
               MOVE INPUT-RECORD TO WS-EXP-TITLE(WS-EXP-IDX)

               MOVE SPACES TO OUTPUT-RECORD
               STRING "Experience #" WS-EXP-IDX
                      " - Company/Organization:"
                   DELIMITED BY SIZE INTO OUTPUT-RECORD
               END-STRING
               PERFORM WRITE-OUTPUT
               PERFORM READ-SAFELY
               IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF
               MOVE INPUT-RECORD TO WS-EXP-COMPANY(WS-EXP-IDX)

               MOVE SPACES TO OUTPUT-RECORD
               STRING "Experience #" WS-EXP-IDX
                      " - Dates (e.g., Summer 2024):"
                   DELIMITED BY SIZE INTO OUTPUT-RECORD
               END-STRING
               PERFORM WRITE-OUTPUT
               PERFORM READ-SAFELY
               IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF
               MOVE INPUT-RECORD TO WS-EXP-DATES(WS-EXP-IDX)

               MOVE SPACES TO OUTPUT-RECORD
               STRING "Experience #" WS-EXP-IDX
                      " - Description (optional, max 100 chars, blank "
                      "to skip):"
                   DELIMITED BY SIZE INTO OUTPUT-RECORD
               END-STRING
               PERFORM WRITE-OUTPUT
               PERFORM READ-SAFELY

               MOVE 100 TO WS-LENGTH-OF-INPUT-MAX
               *> Set for LENGTH-OF-INPUT
               PERFORM INPUT-LENGTH-CHECK
               IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF
               MOVE INPUT-RECORD TO WS-EXP-DESCRIPTION(WS-EXP-IDX)

               ADD 1 TO WS-EXP-COUNT
               ADD 1 TO WS-EXP-IDX
           END-PERFORM.

       *> procedure to get education entries
       GET-EDUCATION.
           MOVE 0 TO WS-EDU-COUNT
           MOVE 1 TO WS-EDU-IDX
           PERFORM UNTIL WS-EDU-IDX > 3 OR INPUT-EOF-FLAG = 'Y'
               MOVE SPACES TO OUTPUT-RECORD
               STRING "Add Education (optional, max 3 entries. Enter "
                      "'DONE' to finish, anything else to continue):"
                   DELIMITED BY SIZE INTO OUTPUT-RECORD
               END-STRING
               PERFORM WRITE-OUTPUT
               PERFORM READ-SAFELY
               IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF
               IF INPUT-RECORD = "DONE"
                   MOVE "DONE" TO OUTPUT-RECORD
                   PERFORM WRITE-OUTPUT
                   EXIT PERFORM
               END-IF

               MOVE SPACES TO OUTPUT-RECORD
               STRING "Education #" WS-EDU-IDX " - Degree:"
                   DELIMITED BY SIZE INTO OUTPUT-RECORD
               END-STRING
               PERFORM WRITE-OUTPUT

               PERFORM READ-SAFELY
               IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF
               MOVE INPUT-RECORD TO WS-EDU-DEGREE(WS-EDU-IDX)

               MOVE SPACES TO OUTPUT-RECORD
               STRING "Education #" WS-EDU-IDX " - University/College:"
                   DELIMITED BY SIZE INTO OUTPUT-RECORD
               END-STRING
               PERFORM WRITE-OUTPUT
               PERFORM READ-SAFELY
               IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF
               MOVE INPUT-RECORD TO WS-EDU-UNIVERSITY(WS-EDU-IDX)

               MOVE SPACES TO OUTPUT-RECORD
               STRING "Education #" WS-EDU-IDX
                      " - Years Attended (e.g., 2023-2025):"
                   DELIMITED BY SIZE INTO OUTPUT-RECORD
               END-STRING
               PERFORM WRITE-OUTPUT
               PERFORM READ-SAFELY
               IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF
               MOVE INPUT-RECORD TO WS-EDU-YEARS(WS-EDU-IDX)

               ADD 1 TO WS-EDU-COUNT
               ADD 1 TO WS-EDU-IDX
           END-PERFORM.

       *> procedure to save profile to Profiles.txt
       SAVE-PROFILE.
           OPEN I-O PROFILES-FILE
           IF WS-PROFILES-STATUS NOT = "00"
               DISPLAY "Error opening file: " WS-PROFILES-STATUS
               STOP RUN
           END-IF

           MOVE WS-PROF-USERNAME TO PROF-USERNAME

           READ PROFILES-FILE
               KEY IS PROF-USERNAME
               INVALID KEY *> username not in profile
                   MOVE WS-PROF-REC TO PROFILE-REC
                   WRITE PROFILE-REC
                       INVALID KEY
                           DISPLAY "Profile write failed: "
                               WS-PROFILES-STATUS
                   END-WRITE
               NOT INVALID KEY
                   MOVE WS-PROF-REC TO PROFILE-REC
                   REWRITE PROFILE-REC
           END-READ
           CLOSE PROFILES-FILE.

       *> procedure to load and view logged-in user's profile
       LOADANDVIEW-PROFILE.
           OPEN I-O PROFILES-FILE
           IF WS-PROFILES-STATUS NOT = "00"
               DISPLAY "Error opening file: " WS-PROFILES-STATUS
               STOP RUN
           END-IF

           MOVE WS-LOGGED-USER TO PROF-USERNAME
           READ PROFILES-FILE
               KEY IS PROF-USERNAME
               INVALID KEY
                   MOVE "No profile data exists for this user."
                       TO OUTPUT-RECORD
                   PERFORM WRITE-OUTPUT
               NOT INVALID KEY
                   MOVE "---------- Your Profile ----------"
                   TO OUTPUT-RECORD
                   PERFORM WRITE-OUTPUT
                   PERFORM VIEW-PROFILE
           END-READ

           CLOSE PROFILES-FILE.

           MOVE "Press ENTER to return to the main menu."
                TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           PERFORM READ-SAFELY
           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF.

       *> procedure to search and view other user profiles
       SEARCHANDVIEW-PROFILE.
           MOVE "------ Find Someone You Know ------" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT

           MOVE SPACES TO OUTPUT-RECORD
           STRING "Enter the Full Name of the user you want to search "
                  "or enter 0 to return to the main menu:"
                DELIMITED BY SIZE INTO OUTPUT-RECORD
           END-STRING
           PERFORM WRITE-OUTPUT

           PERFORM READ-SAFELY
           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF *> check if end

           *> single-step back: user entered 0 instead of a name
           IF INPUT-RECORD(1:1) = '0'
                EXIT PARAGRAPH
              END-IF

           OPEN I-O PROFILES-FILE
           IF WS-PROFILES-STATUS NOT = "00"
               DISPLAY "Error opening file: " WS-PROFILES-STATUS
               STOP RUN
           END-IF
           MOVE 0 TO WS-COUNT
           MOVE 0 TO IDX *> use IDX as count of profiles found

           MOVE INPUT-RECORD TO PROF-FULLNAME
           MOVE INPUT-RECORD TO WS-SEARCH-FULLNAME
           START PROFILES-FILE KEY = PROF-FULLNAME
               INVALID KEY
                   MOVE "No record found with that full name."
                       TO OUTPUT-RECORD
                   PERFORM WRITE-OUTPUT
                   CLOSE PROFILES-FILE
                   EXIT PARAGRAPH
           END-START

           ADD 1 TO WS-COUNT *> at least one profile found (valid key)

           *> Loop READ NEXT RECORD until end reached, where it breaks loop
           MOVE 'N' TO WS-READ-EOF
           PERFORM UNTIL WS-READ-EOF = 'Y'
               READ PROFILES-FILE NEXT RECORD
                   AT END
                       IF WS-COUNT = 1
                           MOVE "No record found with that full name."
                               TO OUTPUT-RECORD
                           PERFORM WRITE-OUTPUT
                       END-IF
                       CLOSE PROFILES-FILE
                       MOVE 'Y' TO WS-READ-EOF
                       EXIT PARAGRAPH
               END-READ
               *> After READ, found profile stored in PROFILE-REC

               IF PROF-USERNAME = WS-LOGGED-USER *> Skip if own profile
                   OR WS-SEARCH-FULLNAME NOT = PROF-FULLNAME *>Not match
                   *> Reduce count to counteract increment count at end
                   SUBTRACT 1 FROM WS-COUNT
               ELSE
                   MOVE "---------- Found Profile ----------"
                   TO OUTPUT-RECORD
                   PERFORM WRITE-OUTPUT
                   PERFORM VIEW-PROFILE
                   *> send connection requests
                   MOVE "1. Send Connection Request" TO OUTPUT-RECORD
                   PERFORM WRITE-OUTPUT
                   MOVE "2. Next match or main menu if no more match"
                           TO OUTPUT-RECORD
                   PERFORM WRITE-OUTPUT
                   MOVE "Enter your choice:" TO OUTPUT-RECORD
                   PERFORM WRITE-OUTPUT

                   PERFORM READ-SAFELY
                   IF INPUT-EOF-FLAG NOT = 'Y'
                        MOVE PROF-USERNAME TO WS-TARGET-USER
                        MOVE PROF-FULLNAME TO WS-TARGET-FULLNAME

                        IF INPUT-RECORD(1:1) = '1'
                            PERFORM SEND-CONNECTION-REQUEST
                        END-IF
                   END-IF
               END-IF
               ADD 1 TO WS-COUNT
           END-PERFORM

           CLOSE PROFILES-FILE.

       *> procedure to display profile info
       VIEW-PROFILE.
           MOVE SPACES TO OUTPUT-RECORD
           STRING "Name: " PROF-FULLNAME DELIMITED BY SIZE
               INTO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT

           MOVE SPACES TO OUTPUT-RECORD
           STRING "University: " PROF-UNIVERSITY
               DELIMITED BY SIZE INTO OUTPUT-RECORD
           END-STRING
           PERFORM WRITE-OUTPUT

           MOVE SPACES TO OUTPUT-RECORD
           STRING "Major: " PROF-MAJOR
               DELIMITED BY SIZE INTO OUTPUT-RECORD
           END-STRING
           PERFORM WRITE-OUTPUT

           MOVE SPACES TO OUTPUT-RECORD
           STRING "Graduation Year: " PROF-GRADYEAR
               DELIMITED BY SIZE INTO OUTPUT-RECORD
           END-STRING
           PERFORM WRITE-OUTPUT

           IF PROF-ABOUTME NOT = SPACES
               MOVE SPACES TO OUTPUT-RECORD
               STRING "About Me: " PROF-ABOUTME
                   DELIMITED BY SIZE INTO OUTPUT-RECORD
               END-STRING
               PERFORM WRITE-OUTPUT
           END-IF

           PERFORM VARYING WS-EXP-IDX FROM 1 BY 1
               UNTIL WS-EXP-IDX > PROF-EXP-COUNT

               IF WS-EXP-IDX = 1
                   MOVE SPACES TO OUTPUT-RECORD
                   MOVE "Experience:" TO OUTPUT-RECORD
                   PERFORM WRITE-OUTPUT
               END-IF
               MOVE SPACES TO OUTPUT-RECORD
               STRING "  Title: " PROF-EXP-TITLE(WS-EXP-IDX)
                   DELIMITED BY SIZE INTO OUTPUT-RECORD
               END-STRING
               PERFORM WRITE-OUTPUT

               MOVE SPACES TO OUTPUT-RECORD
               STRING "  Company: " PROF-EXP-COMPANY(WS-EXP-IDX)
                   DELIMITED BY SIZE INTO OUTPUT-RECORD
               END-STRING
               PERFORM WRITE-OUTPUT

               MOVE SPACES TO OUTPUT-RECORD
               STRING "  Dates: " PROF-EXP-DATES(WS-EXP-IDX)
                   DELIMITED BY SIZE INTO OUTPUT-RECORD
               END-STRING
               PERFORM WRITE-OUTPUT

               IF PROF-EXP-DESCRIPTION(WS-EXP-IDX) NOT = SPACES
                   MOVE SPACES TO OUTPUT-RECORD
                   STRING "  Description: "
                       PROF-EXP-DESCRIPTION(WS-EXP-IDX)
                       DELIMITED BY SIZE INTO OUTPUT-RECORD
                   END-STRING
                   PERFORM WRITE-OUTPUT
               END-IF
           END-PERFORM

           PERFORM VARYING WS-EDU-IDX FROM 1 BY 1
               UNTIL WS-EDU-IDX > PROF-EDU-COUNT

               IF WS-EDU-IDX = 1
                   MOVE SPACES TO OUTPUT-RECORD
                   MOVE "Education:" TO OUTPUT-RECORD
                   PERFORM WRITE-OUTPUT
               END-IF

               MOVE SPACES TO OUTPUT-RECORD
               STRING "  Degree: " PROF-EDU-DEGREE(WS-EDU-IDX)
                   DELIMITED BY SIZE INTO OUTPUT-RECORD
               END-STRING
               PERFORM WRITE-OUTPUT

               MOVE SPACES TO OUTPUT-RECORD
               STRING "  University: " PROF-EDU-UNIVERSITY(WS-EDU-IDX)
                   DELIMITED BY SIZE INTO OUTPUT-RECORD
               END-STRING
               PERFORM WRITE-OUTPUT

               MOVE SPACES TO OUTPUT-RECORD
               STRING "  Years: " PROF-EDU-YEARS(WS-EDU-IDX)
                   DELIMITED BY SIZE INTO OUTPUT-RECORD
               END-STRING
               PERFORM WRITE-OUTPUT
           END-PERFORM.

       *> Calculates the length of INPUT-RECORD in WS-INPUT-LENGTH
       *> This one is complicated, so I will comment extensively
       *> checks the length of INPUT-RECORD in WS-INPUT-LENGTH
       *> and re-prompts if the input exceeds WS-LENGTH-OF-INPUT-MAX
       INPUT-LENGTH-CHECK. *> Must set WS-LENGTH-OF-INPUT-MAX beforehand
           PERFORM WITH TEST AFTER
               UNTIL WS-INPUT-LENGTH <= WS-LENGTH-OF-INPUT-MAX
                  OR INPUT-EOF-FLAG = 'Y'
               *> reset trailing space counter
               MOVE ZERO TO WS-TRAILING-SPACES

               *> count trailing spaces by reversing the input
               INSPECT FUNCTION REVERSE(INPUT-RECORD)
                   TALLYING WS-TRAILING-SPACES FOR LEADING SPACE

               *> effective input length = total - trailing spaces
               COMPUTE WS-INPUT-LENGTH = LENGTH OF INPUT-RECORD
                   - WS-TRAILING-SPACES

               IF INPUT-EOF-FLAG = 'Y'
                   EXIT PARAGRAPH
               END-IF

               *> if too long, tell the user and ask again
               IF WS-INPUT-LENGTH > WS-LENGTH-OF-INPUT-MAX
                   MOVE SPACES TO OUTPUT-RECORD
                   STRING "Input cannot be more than "
                          WS-LENGTH-OF-INPUT-MAX
                          " characters long, as per explicit "
                          "requirements. Please try again."
                       DELIMITED BY SIZE INTO OUTPUT-RECORD
                   END-STRING
                   PERFORM WRITE-OUTPUT

                   *> read a new attempt; this may set INPUT-EOF-FLAG
                   PERFORM READ-SAFELY
               END-IF
           END-PERFORM.


       *> NETWORKING/CONNECTION FEATURES -------------------------------

       *> sends connection request to target user
       SEND-CONNECTION-REQUEST.
           IF WS-LOGGED-USER = WS-TARGET-USER
               MOVE "You cannot send a connection request to yourself."
               TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF

           PERFORM IS-ALREADY-CONNECTED
           IF WS-CONNECTION-FOUND = 'Y'
               MOVE "You are already connected with this user."
               TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF

           PERFORM HAS-PENDING-REQUEST-FROM
           IF WS-REQUEST-FOUND = 'Y'
               MOVE
               "This user has already sent you a connection request."
               TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF

           PERFORM HAS-ALREADY-SENT-REQUEST
           IF WS-REQUEST-FOUND = 'Y'
               STRING "You have already sent a connection request to "
                      "this user." DELIMITED BY SIZE INTO OUTPUT-RECORD
               END-STRING
               PERFORM WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF

           PERFORM SAFE-OPEN-REQUESTS-EXTEND
           MOVE WS-LOGGED-USER TO REQ-SENDER
           MOVE WS-TARGET-USER TO REQ-RECIPIENT
           WRITE REQUEST-REC
           CLOSE REQUESTS-FILE

           MOVE SPACES TO OUTPUT-RECORD
           STRING "Connection request sent to " DELIMITED BY SIZE
               WS-TARGET-USER DELIMITED BY SPACE "." DELIMITED BY SIZE
               INTO OUTPUT-RECORD
           END-STRING
           PERFORM WRITE-OUTPUT
           EXIT PARAGRAPH.

       *> checks if already connected
       IS-ALREADY-CONNECTED.
           MOVE 'N' TO WS-CONNECTION-FOUND
           PERFORM SAFE-OPEN-CONNECTIONS-INPUT
           PERFORM UNTIL WS-CONNECTIONS-STATUS = "10"
               READ CONNECTIONS-FILE NEXT RECORD
                   AT END MOVE "10" TO WS-CONNECTIONS-STATUS
                   NOT AT END
                      *> check if either user is userA or userB
                       IF (CONN-USERA = WS-LOGGED-USER AND
                           CONN-USERB = WS-TARGET-USER) OR
                           (CONN-USERA = WS-TARGET-USER AND
                           CONN-USERB = WS-LOGGED-USER)
                           MOVE 'Y' TO WS-CONNECTION-FOUND
                           MOVE "10" TO WS-CONNECTIONS-STATUS
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CONNECTIONS-FILE.
           EXIT PARAGRAPH.

       *> blocks sending if receipient sent pending request
       HAS-PENDING-REQUEST-FROM.
           MOVE 'N' TO WS-REQUEST-FOUND
           OPEN INPUT REQUESTS-FILE
           IF WS-REQUESTS-STATUS = "35"
                   MOVE 'N' TO WS-REQUEST-FOUND
                   *> Don't need to close - file never opened successfully
                   EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-REQUESTS-STATUS
           *> check if target user has sent logged user a request
           PERFORM UNTIL WS-REQUESTS-STATUS = "10"
                   READ REQUESTS-FILE NEXT RECORD
                        AT END MOVE "10" TO WS-REQUESTS-STATUS
                        NOT AT END
                            IF REQ-SENDER = WS-TARGET-USER AND
                               REQ-RECIPIENT = WS-LOGGED-USER
                                MOVE 'Y' TO WS-REQUEST-FOUND
                                MOVE "10" TO WS-REQUESTS-STATUS
                            END-IF
                   END-READ
           END-PERFORM
           CLOSE REQUESTS-FILE.

       *> locks sending if already sent
       HAS-ALREADY-SENT-REQUEST.
           MOVE 'N' TO WS-REQUEST-FOUND
           OPEN INPUT REQUESTS-FILE
           IF WS-REQUESTS-STATUS = "35"
               MOVE 'N' TO WS-REQUEST-FOUND
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL WS-REQUESTS-STATUS = "10"
               READ REQUESTS-FILE NEXT RECORD
                   AT END MOVE "10" TO WS-REQUESTS-STATUS
                   NOT AT END
                       IF REQ-SENDER = WS-LOGGED-USER AND
                          REQ-RECIPIENT = WS-TARGET-USER
                           MOVE 'Y' TO WS-REQUEST-FOUND
                           MOVE "10" TO WS-REQUESTS-STATUS
                       END-IF
               END-READ
           END-PERFORM
           CLOSE REQUESTS-FILE.
           EXIT PARAGRAPH.

       *> view and process pending connection requests
       VIEW-PENDING-REQUESTS.
           MOVE "--- Pending Connection Requests ---" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT

           MOVE SPACES TO WS-REQUESTS-STATUS
           MOVE 'N' TO WS-REQUEST-FOUND

           OPEN INPUT REQUESTS-FILE

           *> if PendingRequests.txt file does not exist, create it and exit
           IF WS-REQUESTS-STATUS = "35"
               OPEN OUTPUT REQUESTS-FILE
               CLOSE REQUESTS-FILE
               STRING "You have no pending connection requests at this "
                       "time." DELIMITED BY SIZE
                      INTO OUTPUT-RECORD
               END-STRING
               PERFORM WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF

           *> then if file exists and is open, check if it is empty
           MOVE SPACES TO WS-REQUESTS-STATUS
           READ REQUESTS-FILE NEXT RECORD
               AT END
                    STRING "You have no pending connection requests at "
                            "this time." DELIMITED BY SIZE
                          INTO OUTPUT-RECORD
                    END-STRING
                    PERFORM WRITE-OUTPUT
                    CLOSE REQUESTS-FILE
                    EXIT PARAGRAPH
               NOT AT END
                    *> if there is a request continue to processing
                    CONTINUE
           END-READ

           *> File has data, so create temp file and reset for processing
           CLOSE REQUESTS-FILE
           OPEN INPUT REQUESTS-FILE
           OPEN OUTPUT TEMP-REQUESTS-FILE

           *> Reset status for the read loop
           MOVE SPACES TO WS-REQUESTS-STATUS

           *> check for requests where logged in user is recipient
           PERFORM UNTIL WS-REQUESTS-STATUS = "10"
               READ REQUESTS-FILE NEXT RECORD
                     AT END
                         MOVE "10" TO WS-REQUESTS-STATUS
                     NOT AT END
                         *> Check if logged in user is the recipient
                         IF REQ-RECIPIENT = WS-LOGGED-USER
                             MOVE 'Y' TO WS-REQUEST-FOUND
                             MOVE 'N' TO WS-DELETE-REQUEST

                             *> Display sender information
                             MOVE SPACES TO WS-PROFILES-STATUS
                             OPEN I-O PROFILES-FILE
                             IF WS-PROFILES-STATUS = "00"
                                 MOVE REQ-SENDER TO PROF-USERNAME
                                 READ PROFILES-FILE KEY IS PROF-USERNAME
                                     INVALID KEY
                                         *> Profile info not found, show username
                                         MOVE SPACES TO OUTPUT-RECORD
                                         STRING "Request from: "
                                                DELIMITED BY SIZE
                                                REQ-SENDER
                                                DELIMITED BY SPACE
                                             INTO OUTPUT-RECORD
                                         END-STRING
                                         PERFORM WRITE-OUTPUT
                                     NOT INVALID KEY
                                         *> Profile found, show full name
                                         MOVE SPACES TO OUTPUT-RECORD
                                         STRING "Request from: "
                                                DELIMITED BY SIZE
                                                PROF-FIRSTNAME
                                                DELIMITED BY SPACE
                                                " " DELIMITED BY SIZE
                                                PROF-LASTNAME
                                                DELIMITED BY SPACE
                                                " (" DELIMITED BY SIZE
                                                REQ-SENDER
                                                DELIMITED BY SPACE
                                                ")" DELIMITED BY SIZE
                                             INTO OUTPUT-RECORD
                                         END-STRING
                                         PERFORM WRITE-OUTPUT
                                 END-READ
                                 CLOSE PROFILES-FILE
                             ELSE
                                 *> Profile file doesn't exist, show username
                                 MOVE SPACES TO OUTPUT-RECORD
                                 STRING "Request from: "
                                 DELIMITED BY SIZE
                                        REQ-SENDER DELIMITED BY SPACE
                                     INTO OUTPUT-RECORD
                                 END-STRING
                                 PERFORM WRITE-OUTPUT
                             END-IF

                             *> Display options
                             MOVE "1. Accept" TO OUTPUT-RECORD
                             PERFORM WRITE-OUTPUT
                             MOVE "2. Reject" TO OUTPUT-RECORD
                             PERFORM WRITE-OUTPUT
                             MOVE "3. Back to Main Menu"
                             TO OUTPUT-RECORD
                             PERFORM WRITE-OUTPUT
                             MOVE SPACES TO OUTPUT-RECORD
                             STRING "Enter your choice for "
                                    DELIMITED BY SIZE
                                    REQ-SENDER DELIMITED BY SPACE
                                    ":" DELIMITED BY SIZE
                                 INTO OUTPUT-RECORD
                             END-STRING
                             PERFORM WRITE-OUTPUT

                             *> Read user choice
                             PERFORM READ-SAFELY
                             IF INPUT-EOF-FLAG = 'Y'
                                 *> Close all files before exiting
                                 CLOSE REQUESTS-FILE
                                 CLOSE TEMP-REQUESTS-FILE
                                 *> Don't exit here, let cleanup happen
                                 MOVE "10" TO WS-REQUESTS-STATUS
                             ELSE
                                 MOVE INPUT-RECORD(1:1)
                                 TO WS-MENU-CHOICE

                                 *> Process the choice
                                 EVALUATE WS-MENU-CHOICE
                                     WHEN "1"
                                         *> Accept connection request
                                         PERFORM ACCEPT-REQUEST
                                         MOVE SPACES TO OUTPUT-RECORD
                                         STRING "Connection request "
                                                "from "
                                                DELIMITED BY SIZE
                                                REQ-SENDER
                                                DELIMITED BY SPACE
                                                " accepted!"
                                                DELIMITED BY SIZE
                                             INTO OUTPUT-RECORD
                                         END-STRING
                                         PERFORM WRITE-OUTPUT
                                     WHEN "2"
                                         *> Reject connection request
                                         MOVE 'Y' TO WS-DELETE-REQUEST
                                         MOVE SPACES TO OUTPUT-RECORD
                                         STRING "Connection request "
                                                "from "
                                                DELIMITED BY SIZE
                                                REQ-SENDER
                                                DELIMITED BY SPACE
                                                " rejected."
                                                DELIMITED BY SIZE
                                             INTO OUTPUT-RECORD
                                         END-STRING
                                         PERFORM WRITE-OUTPUT
                                     WHEN "3"
                                         *> Back to Main Menu & keep request
                                         MOVE REQ-SENDER
                                         TO TEMP-REQ-SENDER
                                         MOVE REQ-RECIPIENT
                                         TO TEMP-REQ-RECIPIENT
                                         WRITE TEMP-REQUESTS-RECORD
                                         *> Exit the loop to go back
                                         MOVE "10" TO WS-REQUESTS-STATUS
                                     WHEN OTHER
                                         *> Invalid choice, keep request
                                         MOVE "Invalid choice."
                                         TO OUTPUT-RECORD
                                         PERFORM WRITE-OUTPUT
                                         *> Write to temp file
                                         MOVE REQ-SENDER
                                         TO TEMP-REQ-SENDER
                                         MOVE REQ-RECIPIENT
                                         TO TEMP-REQ-RECIPIENT
                                         WRITE TEMP-REQUESTS-RECORD
                                 END-EVALUATE
                             END-IF
                         ELSE
                             *> Request not for logged user, preserve it
                             MOVE REQ-SENDER TO TEMP-REQ-SENDER
                             MOVE REQ-RECIPIENT TO TEMP-REQ-RECIPIENT
                             WRITE TEMP-REQUESTS-RECORD
                         END-IF
               END-READ
           END-PERFORM

           *> Close both files
           CLOSE REQUESTS-FILE
           CLOSE TEMP-REQUESTS-FILE

           *> Only update file if we didn't hit EOF during processing
           IF INPUT-EOF-FLAG NOT = 'Y'
               *> Replace original file with updated temp file
               OPEN OUTPUT REQUESTS-FILE
               OPEN INPUT TEMP-REQUESTS-FILE

               *> Copy all records from temp to original
               MOVE SPACES TO WS-TEMP-REQUESTS-STATUS
               PERFORM UNTIL WS-TEMP-REQUESTS-STATUS = "10"
                    READ TEMP-REQUESTS-FILE
                         AT END
                             MOVE "10" TO WS-TEMP-REQUESTS-STATUS
                         NOT AT END
                             MOVE TEMP-REQ-SENDER TO REQ-SENDER
                             MOVE TEMP-REQ-RECIPIENT TO REQ-RECIPIENT
                             WRITE REQUEST-REC
                    END-READ
               END-PERFORM

               *> Close both files
               CLOSE TEMP-REQUESTS-FILE
               CLOSE REQUESTS-FILE
           END-IF

           *> Display message if no requests were found
           IF WS-REQUEST-FOUND NOT = 'Y'
               STRING "You have no pending connection requests at this "
                        "time." DELIMITED BY SIZE
                     INTO OUTPUT-RECORD
               END-STRING
               PERFORM WRITE-OUTPUT
           END-IF.

       *> procedure to accept a connection request
       ACCEPT-REQUEST.
           *> save the request info
           MOVE REQ-SENDER TO WS-TEMP-SENDER
           MOVE REQ-RECIPIENT TO WS-TEMP-RECIPIENT

           *> add connection to both users' connection files
           *> add sender to recipient's connections
           MOVE WS-TEMP-RECIPIENT TO CONN-USERA
           OPEN EXTEND CONNECTIONS-FILE
           MOVE WS-TEMP-SENDER TO CONN-USERB
           WRITE CONNECTION-REC
           CLOSE CONNECTIONS-FILE
           *> add recipient to sender's connections
           MOVE WS-TEMP-SENDER TO CONN-USERA
           OPEN EXTEND CONNECTIONS-FILE
           MOVE WS-TEMP-RECIPIENT TO CONN-USERB
           WRITE CONNECTION-REC
           CLOSE CONNECTIONS-FILE

           *> mark request for deletion
           MOVE 'Y' TO WS-DELETE-REQUEST.

       *> view user's connections
       VIEW-MY-NETWORK.
           MOVE "---------- Your Network ----------" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           PERFORM SAFE-OPEN-CONNECTIONS-INPUT
           MOVE 'N' TO WS-CONNECTION-FOUND
           MOVE 0 TO WS-COUNT

           *> read through connections file to find user's connections
           PERFORM UNTIL WS-CONNECTIONS-STATUS = "10"
               READ CONNECTIONS-FILE NEXT RECORD
                    AT END MOVE "10" TO WS-CONNECTIONS-STATUS
                    NOT AT END
                        *> only check if logged user is USERA (bc B->A is same)
                        IF CONN-USERA = WS-LOGGED-USER
                            MOVE 'Y' TO WS-CONNECTION-FOUND
                            ADD 1 TO WS-COUNT
                            MOVE CONN-USERB TO WS-TARGET-USER

                            *> lookup connection's profile info
                            OPEN I-O PROFILES-FILE
                            IF WS-PROFILES-STATUS = "00"
                                MOVE WS-TARGET-USER TO PROF-USERNAME
                                READ PROFILES-FILE
                                KEY IS PROF-USERNAME
                                    INVALID KEY
                                       *> if profile not found, display username
                                        MOVE SPACES TO OUTPUT-RECORD
                                        STRING "Connected with: "
                                               DELIMITED BY SIZE
                                              WS-TARGET-USER
                                              DELIMITED BY SPACE
                                           INTO OUTPUT-RECORD
                                        END-STRING
                                        PERFORM WRITE-OUTPUT
                                    NOT INVALID KEY
                                        *> if found, show full name, uni, major
                                        MOVE SPACES TO OUTPUT-RECORD
                                        STRING "Connected with: "
                                               DELIMITED BY SIZE
                                               PROF-FIRSTNAME
                                               DELIMITED BY SPACE
                                               " " DELIMITED BY SIZE
                                               PROF-LASTNAME
                                               DELIMITED BY SPACE
                                               " (University: "
                                               DELIMITED BY SIZE
                                               PROF-UNIVERSITY
                                               DELIMITED BY SPACE
                                               ", Major: "
                                               DELIMITED BY SIZE
                                               PROF-MAJOR
                                               DELIMITED BY SPACE
                                               ")" DELIMITED BY SIZE
                                           INTO OUTPUT-RECORD
                                        END-STRING
                                        PERFORM WRITE-OUTPUT
                                END-READ
                                CLOSE PROFILES-FILE
                            *> if profile file doesn't exist, show only username
                            ELSE
                                MOVE SPACES TO OUTPUT-RECORD
                                STRING "Connected with: "
                                       DELIMITED BY SIZE
                                       WS-TARGET-USER
                                       DELIMITED BY SPACE
                                   INTO OUTPUT-RECORD
                                END-STRING
                                PERFORM WRITE-OUTPUT
                            END-IF
                        END-IF
               END-READ
           END-PERFORM
           CLOSE CONNECTIONS-FILE

           *> if no connections found, display message
           IF WS-CONNECTION-FOUND NOT = 'Y'
               MOVE "You have no connections yet." TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
           END-IF.

       *> safely opens requests file for appending
       SAFE-OPEN-REQUESTS-EXTEND.
           OPEN EXTEND REQUESTS-FILE
           IF WS-REQUESTS-STATUS = "35"
               OPEN OUTPUT REQUESTS-FILE
               CLOSE REQUESTS-FILE
               OPEN EXTEND REQUESTS-FILE
           END-IF.
           EXIT PARAGRAPH.

       *> safely opens connections file for input
       SAFE-OPEN-CONNECTIONS-INPUT.
           OPEN INPUT CONNECTIONS-FILE
           IF WS-CONNECTIONS-STATUS = "35"
               OPEN OUTPUT CONNECTIONS-FILE
               CLOSE CONNECTIONS-FILE
               OPEN INPUT CONNECTIONS-FILE
           END-IF.
           EXIT PARAGRAPH.

       *> JOB/INTERNSHIP FEATURES --------------------------------------

       *> displays job search/internship menu
       JOB-SEARCH-MENU.
           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF
           MOVE "--- Job Search/Internship Menu ---" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE "1. Post a Job/Internship" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE "2. Browse Jobs/Internships" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE "3. View My Applications" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE "4. Back to Main Menu" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE "Enter your choice:" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT

           PERFORM READ-SAFELY
           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF

           MOVE INPUT-RECORD(1:3) TO WS-JOB-CHOICE
           EVALUATE WS-JOB-CHOICE
               WHEN 1
                   PERFORM POST-JOB
                   IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF
                   PERFORM JOB-SEARCH-MENU
               WHEN 2
                   PERFORM BROWSE-JOBS
                   IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF
                   PERFORM JOB-SEARCH-MENU
               WHEN 3
                   PERFORM VIEW-MY-APPLICATIONS
                   IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF
                   PERFORM JOB-SEARCH-MENU
               WHEN 4
                   CONTINUE
               WHEN OTHER
                   MOVE "Invalid choice." TO OUTPUT-RECORD
                   PERFORM WRITE-OUTPUT
                   PERFORM JOB-SEARCH-MENU
           END-EVALUATE.

       *> procedure to post a new job/internship
       POST-JOB.
           MOVE "---- Post a New Job/Internship ----" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT

           *> get job title
           MOVE "Enter Job Title:" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           PERFORM READ-SAFELY
           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF
           MOVE 100 TO WS-LENGTH-OF-INPUT-MAX
           PERFORM INPUT-LENGTH-CHECK
           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF

           *> check if title is empty
           IF FUNCTION TRIM(INPUT-RECORD) = SPACES
               MOVE "Job Title is a required field. Try again."
               TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               CLOSE INPUT-FILE
               CLOSE OUTPUT-FILE
               STOP RUN
           END-IF
           MOVE INPUT-RECORD(1:100) TO WS-JOB-TITLE

           *> get job description
           MOVE "Enter Description (max 200 chars):" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           PERFORM READ-SAFELY
           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF
           MOVE 200 TO WS-LENGTH-OF-INPUT-MAX
           PERFORM INPUT-LENGTH-CHECK
           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF

           *> check if description is empty
           IF FUNCTION TRIM(INPUT-RECORD) = SPACES
               MOVE "Job Description is a required field. Try again."
               TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               CLOSE INPUT-FILE
               CLOSE OUTPUT-FILE
               STOP RUN
           END-IF
           MOVE INPUT-RECORD(1:200) TO WS-JOB-DESCRIPTION

           *> get employer for job
           MOVE "Enter Employer Name:" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           PERFORM READ-SAFELY
           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF
           MOVE 100 TO WS-LENGTH-OF-INPUT-MAX
           PERFORM INPUT-LENGTH-CHECK
           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF

           *> check if employer is empty
           IF FUNCTION TRIM(INPUT-RECORD) = SPACES
               MOVE "Employer name is a required field. Try again."
               TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               CLOSE INPUT-FILE
               CLOSE OUTPUT-FILE
               STOP RUN
           END-IF
           MOVE INPUT-RECORD(1:100) TO WS-JOB-EMPLOYER

           *> get job location
           MOVE "Enter Location:" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           PERFORM READ-SAFELY
           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF
           MOVE 100 TO WS-LENGTH-OF-INPUT-MAX
           PERFORM INPUT-LENGTH-CHECK
           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF

           *> check if location is empty
           IF FUNCTION TRIM(INPUT-RECORD) = SPACES
               MOVE "Job Location is a required field. Try again."
               TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               CLOSE INPUT-FILE
               CLOSE OUTPUT-FILE
               STOP RUN
           END-IF
           MOVE INPUT-RECORD(1:100) TO WS-JOB-LOCATION

           *> get salary (optional, but needs input)
           MOVE "Enter Salary (optional, enter 'NONE' to skip):"
           TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           PERFORM READ-SAFELY
           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF
           MOVE 50 TO WS-LENGTH-OF-INPUT-MAX
           PERFORM INPUT-LENGTH-CHECK
           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF

           *> check if salary field is empty
           IF FUNCTION TRIM(INPUT-RECORD) = SPACES
               MOVE "Invalid input (enter 'NONE' to skip). Try again."
               TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               CLOSE INPUT-FILE
               CLOSE OUTPUT-FILE
               STOP RUN
           END-IF
           MOVE INPUT-RECORD(1:50) TO WS-JOB-SALARY

           *> check for duplicate job posting
           PERFORM CHECK-DUPLICATE-JOB
           IF WS-FOUND = 'Y'
               MOVE SPACES TO OUTPUT-RECORD
               STRING "A job with this title, employer, and location "
                      "already exists."
                   DELIMITED BY SIZE INTO OUTPUT-RECORD
               END-STRING
               PERFORM WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF

           *> write job to job.txt
           OPEN EXTEND JOBS-FILE
           IF WS-JOBS-STATUS = "35"
               OPEN OUTPUT JOBS-FILE
               CLOSE JOBS-FILE
               OPEN EXTEND JOBS-FILE
           END-IF

           MOVE WS-LOGGED-USER TO JOB-POSTER
           MOVE WS-JOB-TITLE TO JOB-TITLE
           MOVE WS-JOB-DESCRIPTION TO JOB-DESCRIPTION
           MOVE WS-JOB-EMPLOYER TO JOB-EMPLOYER
           MOVE WS-JOB-LOCATION TO JOB-LOCATION
           MOVE WS-JOB-SALARY TO JOB-SALARY
           WRITE JOB-RECORD
           CLOSE JOBS-FILE

           MOVE "Job posted successfully!" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT.

       *> checks for duplicate job postings
       CHECK-DUPLICATE-JOB.
           MOVE 'N' TO WS-FOUND
           MOVE 'N' TO EOF
           OPEN INPUT JOBS-FILE
           IF WS-JOBS-STATUS = "35"
               CLOSE JOBS-FILE
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL EOF = 'Y'
               READ JOBS-FILE
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       IF JOB-TITLE = WS-JOB-TITLE AND
                          JOB-EMPLOYER = WS-JOB-EMPLOYER AND
                          JOB-LOCATION = WS-JOB-LOCATION
                           MOVE 'Y' TO WS-FOUND
                           MOVE 'Y' TO EOF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE JOBS-FILE.

       *> procedure to browse available jobs/internships
       BROWSE-JOBS.
           MOVE "----- Available Job Listings -----" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT

            OPEN INPUT JOBS-FILE
           IF WS-JOBS-STATUS NOT = "00" AND WS-JOBS-STATUS NOT = "05"
               MOVE "Error opening jobs file." TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF

           IF WS-JOBS-STATUS = "05"
               MOVE "No jobs available at this time." TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               CLOSE JOBS-FILE
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-JOB-COUNT
           MOVE 'N' TO EOF

           PERFORM UNTIL EOF = 'Y'
               READ JOBS-FILE
                   AT END
                      MOVE 'Y' TO EOF
                   NOT AT END
                       ADD 1 TO WS-JOB-COUNT
                       MOVE SPACES TO OUTPUT-RECORD
                       STRING WS-JOB-COUNT ". "
                           FUNCTION TRIM(JOB-TITLE) " at "
                           FUNCTION TRIM(JOB-EMPLOYER) " ("
                           FUNCTION TRIM(JOB-LOCATION) ")"
                           DELIMITED BY SIZE INTO OUTPUT-RECORD
                       END-STRING
                       PERFORM WRITE-OUTPUT
              END-READ
           END-PERFORM

           CLOSE JOBS-FILE

           IF WS-JOB-COUNT = 0
               MOVE "No jobs available." TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF

           MOVE "Enter job number to view details, or 0 to go back:"
               TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT

           PERFORM READ-SAFELY
           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF
           MOVE INPUT-RECORD(1:3) TO WS-JOB-CHOICE

           IF WS-JOB-CHOICE = 0
               EXIT PARAGRAPH
           END-IF

           IF WS-JOB-CHOICE > 0 AND WS-JOB-CHOICE <= WS-JOB-COUNT
               PERFORM VIEW-JOB-DETAILS
               IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF
           ELSE
               MOVE "Invalid job number." TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
           END-IF

           PERFORM BROWSE-JOBS.

       *> procedure to view job details and apply
       VIEW-JOB-DETAILS.
           OPEN INPUT JOBS-FILE
           IF WS-JOBS-STATUS NOT = "00"
               MOVE "Error opening jobs file." TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-COUNT
           MOVE 'N' TO EOF

           PERFORM UNTIL EOF = 'Y' OR WS-COUNT = WS-JOB-CHOICE
               READ JOBS-FILE
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       ADD 1 TO WS-COUNT
               END-READ
           END-PERFORM

           IF WS-COUNT = WS-JOB-CHOICE
               MOVE "----------- Job Details -----------"
               TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               MOVE SPACES TO OUTPUT-RECORD
               STRING "Title: " JOB-TITLE DELIMITED BY SIZE
                  INTO OUTPUT-RECORD
               END-STRING
               PERFORM WRITE-OUTPUT
               MOVE SPACES TO OUTPUT-RECORD
               STRING "Description: " JOB-DESCRIPTION
                   DELIMITED BY SIZE INTO OUTPUT-RECORD
               END-STRING
               PERFORM WRITE-OUTPUT
               MOVE SPACES TO OUTPUT-RECORD
               STRING "Employer: " JOB-EMPLOYER
                   DELIMITED BY SIZE INTO OUTPUT-RECORD
               END-STRING
               PERFORM WRITE-OUTPUT
               MOVE SPACES TO OUTPUT-RECORD
               STRING "Location: " JOB-LOCATION
                   DELIMITED BY SIZE INTO OUTPUT-RECORD
               END-STRING
               PERFORM WRITE-OUTPUT

               IF JOB-SALARY NOT = SPACES
                   MOVE SPACES TO OUTPUT-RECORD
                   STRING "Salary: " JOB-SALARY
                       DELIMITED BY SIZE INTO OUTPUT-RECORD
                   END-STRING
                   PERFORM WRITE-OUTPUT
               END-IF

               MOVE "---" TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               MOVE "1. Apply for this Job" TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               MOVE "2. Back to Job List" TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               MOVE "Enter your choice:" TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT

               PERFORM READ-SAFELY
               IF INPUT-EOF-FLAG = 'Y'
                   CLOSE JOBS-FILE
                   EXIT PARAGRAPH
               END-IF

               MOVE INPUT-RECORD(1:1) TO WS-APPLY-CHOICE
               IF WS-APPLY-CHOICE = 1
                   PERFORM APPLY-FOR-JOB
               END-IF
           END-IF

           CLOSE JOBS-FILE.

       *> procedure to apply for a job
       APPLY-FOR-JOB.
           *> check for duplicate application
           PERFORM CHECK-DUPLICATE-APPLICATION
           IF WS-FOUND = 'Y'
               MOVE "You have already applied to this job."
                   TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF

           OPEN EXTEND APPLICATIONS-FILE
           IF WS-APPLICATIONS-STATUS NOT = "00" AND
           WS-APPLICATIONS-STATUS NOT = "05"
               MOVE "Error opening applications file." TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               CLOSE APPLICATIONS-FILE
               EXIT PARAGRAPH
           END-IF

           MOVE WS-LOGGED-USER TO APP-USERNAME
           MOVE JOB-TITLE TO APP-JOB-TITLE
           MOVE JOB-EMPLOYER TO APP-EMPLOYER
           MOVE JOB-LOCATION TO APP-LOCATION

           WRITE APPLICATION-RECORD

           CLOSE APPLICATIONS-FILE

           MOVE SPACES TO OUTPUT-RECORD
           STRING "Your application for " FUNCTION TRIM(JOB-TITLE)
               " at " FUNCTION TRIM(JOB-EMPLOYER)
                " has been submitted."
               DELIMITED BY SIZE INTO OUTPUT-RECORD
           END-STRING
           PERFORM WRITE-OUTPUT.

       *> procedure to check for duplicate job postings
       CHECK-DUPLICATE-APPLICATION.
           MOVE 'N' TO WS-FOUND
           MOVE 'N' TO EOF
           OPEN INPUT APPLICATIONS-FILE
           IF WS-APPLICATIONS-STATUS = "35"
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL EOF = 'Y'
               READ APPLICATIONS-FILE
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       IF APP-USERNAME = WS-LOGGED-USER AND
                          APP-JOB-TITLE = JOB-TITLE AND
                          APP-EMPLOYER = JOB-EMPLOYER AND
                          APP-LOCATION = JOB-LOCATION
                           MOVE 'Y' TO WS-FOUND
                           MOVE 'Y' TO EOF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE APPLICATIONS-FILE.

       *> procedure to view user's job applications
       VIEW-MY-APPLICATIONS.
           MOVE "------ Your Job Applications ------" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE SPACES TO OUTPUT-RECORD
           STRING "Application Summary for " WS-LOGGED-USER
               DELIMITED BY SIZE INTO OUTPUT-RECORD
           END-STRING
           PERFORM WRITE-OUTPUT
           MOVE "---" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT

           OPEN INPUT APPLICATIONS-FILE
           IF WS-APPLICATIONS-STATUS NOT = "00" AND
           WS-APPLICATIONS-STATUS NOT = "05"
               MOVE "Error opening applications file." TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               CLOSE APPLICATIONS-FILE
               EXIT PARAGRAPH
           END-IF

           IF WS-APPLICATIONS-STATUS = "05"
               MOVE "You have not applied to any jobs yet."
               TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               CLOSE APPLICATIONS-FILE
               EXIT PARAGRAPH
           END-IF

           DELETE FILE APPLICATIONS-REPORT-FILE.
           CLOSE APPLICATIONS-REPORT-FILE.
           OPEN OUTPUT APPLICATIONS-REPORT-FILE
           IF WS-APPLICATIONS-REPORT-STATUS NOT = "00"
               STRING "Error opening application report file "
                   WS-APPLICATIONS-REPORT-STATUS
                   DELIMITED BY SIZE INTO OUTPUT-RECORD
               END-STRING
               PERFORM WRITE-OUTPUT
               CLOSE APPLICATIONS-REPORT-FILE
               CLOSE APPLICATIONS-FILE
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO AppR-PageCount
           PERFORM PrintPageHeading

           MOVE 0 TO WS-APP-COUNT
           MOVE 'N' TO EOF

           PERFORM UNTIL EOF = 'Y'
               READ APPLICATIONS-FILE
                   AT END
                       MOVE 'Y' TO EOF
                       SET WSEOF TO TRUE
                   NOT AT END
                       IF APP-USERNAME = WS-LOGGED-USER
                           ADD 1 TO WS-APP-COUNT
                           MOVE SPACES TO OUTPUT-RECORD
                           STRING "Job Title: " APP-JOB-TITLE
                               DELIMITED BY SIZE INTO OUTPUT-RECORD
                           END-STRING
                           PERFORM WRITE-OUTPUT
                           MOVE SPACES TO OUTPUT-RECORD
                           STRING "Employer: " APP-EMPLOYER
                               DELIMITED BY SIZE INTO OUTPUT-RECORD
                           END-STRING
                           PERFORM WRITE-OUTPUT
                           MOVE SPACES TO OUTPUT-RECORD
                           STRING "Location: " APP-LOCATION
                               DELIMITED BY SIZE INTO OUTPUT-RECORD
                           END-STRING
                           PERFORM WRITE-OUTPUT
                           MOVE "---" TO OUTPUT-RECORD
                           PERFORM WRITE-OUTPUT
                           PERFORM PrintReportBody
                       END-IF
               END-READ
           END-PERFORM

           PERFORM PrintReportEnding
           CLOSE APPLICATIONS-FILE
           CLOSE APPLICATIONS-REPORT-FILE
           IF WS-APP-COUNT = 0
               MOVE "You have not applied to any jobs yet."
                  TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
           END-IF

           MOVE SPACES TO OUTPUT-RECORD
           MOVE WS-APP-COUNT TO WS-APP-COUNT-DISPLAY
           STRING "Total Applications: " WS-APP-COUNT-DISPLAY
               DELIMITED BY SIZE INTO OUTPUT-RECORD
           END-STRING
           PERFORM WRITE-OUTPUT.

       *> procedure to print page heading
       PrintPageHeading.
            MOVE WS-LOGGED-USER TO AppR-PrnUserName
            WRITE APPLICATION-REPORT-RECORD FROM AppR-PageHeading
                AFTER ADVANCING Page
            END-WRITE
            WRITE APPLICATION-REPORT-RECORD FROM AppR-Headers
                AFTER ADVANCING 5 LINES
            END-WRITE
            WRITE APPLICATION-REPORT-RECORD FROM AppR-HorizontalLine
                AFTER ADVANCING 1 LINES
            END-WRITE
            MOVE 6 TO AppR-LineCount
            ADD 1 TO AppR-PageCount.

       *> procedure to print report body
       PrintReportBody.
           IF AppR-NewPageRequired
               MOVE AppR-PageCount TO AppR-PrnPageNum
               WRITE APPLICATION-REPORT-RECORD FROM AppR-PageFooting
                   AFTER ADVANCING 5 LINES
               END-WRITE
               PERFORM PrintPageHeading
           END-IF
           MOVE APP-JOB-TITLE TO AppR-PrnJobTitle
           MOVE APP-EMPLOYER TO AppR-PrnEmployer
           MOVE APP-LOCATION TO AppR-PrnLocation
           WRITE APPLICATION-REPORT-RECORD FROM AppR-JobDetailLine
               AFTER ADVANCING 1 LINES
           END-WRITE
           ADD 1 TO AppR-LineCount.

       *> procedure to print report ending
       PrintReportEnding.
           MOVE WS-APP-COUNT TO AppR-TotalApplicationsCount
           WRITE APPLICATION-REPORT-RECORD
               FROM AppR-TotalApplicationsLine
               AFTER ADVANCING 5 LINES
           END-WRITE.
           WRITE APPLICATION-REPORT-RECORD FROM AppR-ReportFooting
               AFTER ADVANCING 1 LINES
           END-WRITE.

       *> MESSAGING FEATURES -------------------------------------------

       *> procedure to show messages menu
       MESSAGES-MENU.
           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF
           MOVE "---------- Messages Menu ----------" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE "1. Send a New Message" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE "2. View My Messages" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE "3. Back to Main Menu" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE "Enter your choice:" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT

           PERFORM READ-SAFELY
           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF

           MOVE INPUT-RECORD(1:1) TO WS-CHOICE
           EVALUATE WS-CHOICE
               WHEN 1
                   PERFORM SEND-NEW-MESSAGE
                   PERFORM MESSAGES-MENU
               WHEN 2
                   PERFORM VIEW-MY-MESSAGES
                   PERFORM MESSAGES-MENU
               WHEN 3
                   CONTINUE
               WHEN OTHER
                   MOVE "Invalid choice." TO OUTPUT-RECORD
                   PERFORM WRITE-OUTPUT
                   PERFORM MESSAGES-MENU
           END-EVALUATE.

       *> procedure to send a new message
       SEND-NEW-MESSAGE.
           MOVE "Enter recipient's username (must be a connection):"
               TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           PERFORM READ-SAFELY
           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF *> check if end
           MOVE INPUT-RECORD(1:20) TO WS-TARGET-USERNAME

           *> Check if username is a connection
           MOVE 'N' TO WS-FOUND *> initialize found flag to No
           MOVE 'N' TO EOF *> initialize EOF flag to No
           PERFORM SAFE-OPEN-CONNECTIONS-INPUT *> creates file if 35
           PERFORM UNTIL EOF = 'Y' OR WS-FOUND = 'Y'  *> until EOF/found
               READ CONNECTIONS-FILE  *> read next acc
                   AT END MOVE 'Y' TO EOF *> if EOF change flag to Yes
                   NOT AT END  *> if entire file was read
                       IF CONN-USERA = WS-LOGGED-USER AND
                           CONN-USERB = WS-TARGET-USERNAME
                           MOVE 'Y' TO WS-FOUND *> found flag to Yes
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CONNECTIONS-FILE

           IF WS-FOUND = 'N'
               MOVE "User not found in your network."
                   TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF

           *> Loop to get valid message input
           PERFORM WITH TEST AFTER UNTIL WS-INPUT-LENGTH <= 200
                   OR INPUT-EOF-FLAG = 'Y'
               MOVE "Enter your message (max 200 chars):"
                   TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               PERFORM READ-SAFELY
               IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF

               MOVE 200 TO WS-LENGTH-OF-INPUT-MAX
               *> Set for LENGTH-OF-INPUT

               *> Calculate input length
               MOVE ZERO TO WS-TRAILING-SPACES
               INSPECT FUNCTION REVERSE(INPUT-RECORD)
                   TALLYING WS-TRAILING-SPACES FOR LEADING SPACE
               COMPUTE WS-INPUT-LENGTH = LENGTH OF INPUT-RECORD
                   - WS-TRAILING-SPACES

               *> Check if input is too long
               IF WS-INPUT-LENGTH > WS-LENGTH-OF-INPUT-MAX
                   MOVE SPACES TO OUTPUT-RECORD
                   STRING "Input cannot be more than "
                          WS-LENGTH-OF-INPUT-MAX
                          " characters long, as per explicit "
                          "requirements. Please try again."
                       DELIMITED BY SIZE INTO OUTPUT-RECORD
                   END-STRING
                   PERFORM WRITE-OUTPUT
               END-IF
           END-PERFORM

           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF
           MOVE INPUT-RECORD TO WS-MSG-CONTENT

           *> Save message to Messages.txt
           OPEN EXTEND MESSAGES-FILE
           IF WS-MESSAGES-STATUS NOT = "00" AND
              WS-MESSAGES-STATUS NOT = "05"
               MOVE "Error opening messages file." TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               CLOSE MESSAGES-FILE
               EXIT PARAGRAPH
           END-IF

           MOVE WS-LOGGED-USER TO MSG-SENDER
           MOVE WS-TARGET-USERNAME TO MSG-RECEIVER
           MOVE WS-MSG-CONTENT TO MSG-CONTENT
           ACCEPT WS-DATE FROM DATE
           ACCEPT WS-TIME FROM TIME
           STRING WS-DATE DELIMITED BY SIZE
               WS-TIME DELIMITED BY SIZE
               INTO WS-MSG-TIMESTAMP
           END-STRING
           MOVE WS-MSG-TIMESTAMP TO MSG-TIMESTAMP

           WRITE MESSAGE-RECORD

           CLOSE MESSAGES-FILE
           STRING "Message sent to "
               FUNCTION TRIM(WS-TARGET-USERNAME TRAILING)
               " successfully."
               DELIMITED BY SIZE
               INTO OUTPUT-RECORD
           END-STRING
           PERFORM WRITE-OUTPUT.

       *> procedure to view logged-in user's messages
       VIEW-MY-MESSAGES.
           MOVE "---------- Your Messages ----------" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE 0 TO WS-COUNT
           OPEN INPUT MESSAGES-FILE
           IF WS-MESSAGES-STATUS = "35"
               OPEN OUTPUT MESSAGES-FILE
               CLOSE MESSAGES-FILE
               MOVE "You have no messages at this time."
                   TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO EOF

           PERFORM UNTIL EOF = 'Y'
               READ MESSAGES-FILE NEXT RECORD
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       IF MSG-RECEIVER = WS-LOGGED-USER
                           ADD 1 TO WS-COUNT
                           MOVE SPACES TO OUTPUT-RECORD
                           STRING "From: " DELIMITED BY SIZE
                                  MSG-SENDER DELIMITED BY SPACE
                               INTO OUTPUT-RECORD
                           END-STRING
                           PERFORM WRITE-OUTPUT
                           MOVE SPACES TO OUTPUT-RECORD
                           STRING "Message: " DELIMITED BY SIZE
                                  MSG-CONTENT DELIMITED BY SIZE
                               INTO OUTPUT-RECORD
                           END-STRING
                           PERFORM WRITE-OUTPUT
                           IF MSG-TIMESTAMP NOT = SPACES
                               PERFORM FORMAT-TIMESTAMP
                               MOVE SPACES TO OUTPUT-RECORD
                               STRING "Sent: " DELIMITED BY SIZE
                                      WS-FORMATTED-TIMESTAMP
                                      DELIMITED BY SIZE
                                   INTO OUTPUT-RECORD
                               END-STRING
                               PERFORM WRITE-OUTPUT
                           END-IF
                           MOVE "---" TO OUTPUT-RECORD
                           PERFORM WRITE-OUTPUT
                       END-IF
               END-READ
           END-PERFORM

           CLOSE MESSAGES-FILE
           IF WS-COUNT = 0
               MOVE "You have no messages at this time."
                   TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
           END-IF.

       *> procedure to format timestamp
       FORMAT-TIMESTAMP.
           *> Parse timestamp from MSG-TIMESTAMP (YYMMDD  HHMMSS)
           MOVE MSG-TIMESTAMP(1:2) TO WS-TS-YEAR
           MOVE MSG-TIMESTAMP(3:2) TO WS-TS-MONTH
           MOVE MSG-TIMESTAMP(5:2) TO WS-TS-DAY
           MOVE MSG-TIMESTAMP(9:2) TO WS-TS-HOUR
           MOVE MSG-TIMESTAMP(11:2) TO WS-TS-MINUTE
           IF WS-TS-YEAR < "50"
               STRING "20" DELIMITED BY SIZE
                      WS-TS-YEAR DELIMITED BY SIZE
                   INTO WS-TS-FULL-YEAR
               END-STRING
           ELSE
               STRING "19" DELIMITED BY SIZE
                      WS-TS-YEAR DELIMITED BY SIZE
                   INTO WS-TS-FULL-YEAR
               END-STRING
           END-IF
           STRING WS-TS-FULL-YEAR DELIMITED BY SIZE
                  "-" DELIMITED BY SIZE
                  WS-TS-MONTH DELIMITED BY SIZE
                  "-" DELIMITED BY SIZE
                  WS-TS-DAY DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  WS-TS-HOUR DELIMITED BY SIZE
                  ":" DELIMITED BY SIZE
                  WS-TS-MINUTE DELIMITED BY SIZE
               INTO WS-FORMATTED-TIMESTAMP
           END-STRING.

       *> SKILLS MENU --------------------------------------------------

       *> procedure to show learn skill menu
       LEARN-SKILL-MENU. *> shows skills to learn
           MOVE "-------- Learn a New Skill --------" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE "1. Strength" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE "2. Dexterity" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE "3. Intelligence" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE "4. Wisdom" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE "5. Charisma" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE "6. Back to Main Menu" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT
           MOVE "Enter your choice:" TO OUTPUT-RECORD
           PERFORM WRITE-OUTPUT

           PERFORM READ-SAFELY
           IF INPUT-EOF-FLAG = 'Y' EXIT PARAGRAPH END-IF *> check if end

           MOVE INPUT-RECORD(1:1) TO WS-SKILL-CHOICE  *> get first char
           IF WS-SKILL-CHOICE = 6  *> if user chose 6
               CONTINUE
           ELSE *> if user chose skill (1-5)
               MOVE "This skill is under construction." TO OUTPUT-RECORD
               PERFORM WRITE-OUTPUT
               PERFORM LEARN-SKILL-MENU *> go back to skill options
           END-IF.

       END PROGRAM INCOLLEGE.
