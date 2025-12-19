Compile:
  cobc -x -Wall InCollege.cob

Prepare inputs:
- Place ONE of the provided input files in the working directory with the exact name expected by the program:
  InCollege-Input.txt
- The program will also create/update:
  InCollege-Output.txt   (mirror of screen output)
  Accounts.txt           (persistent account storage)
  Profiles.txt           (persistent profiles storage (binary file, only partially human-readable))
  PendingRequests.txt    (persistent pending requests storage)
  TempPending.txt        (persistent temporary pending requests storage before overwriting)
  Connections.txt        (persistent connections storage)
  Jobs.txt               (persistent job storage)
  Applications.txt       (persistent applications storage)
  ApplicationsReport.txt (report of applications)
  Messages.txt           (persistent messages storage)

Run program:
    ./InCollege

Testing:
- Copy one of the test provided inside 'InCollege-Testing.txt' to 'InCollege-Input.txt'
- Run the program

Makefile:
- make (default, compiles and runs the program)
- make fresha (deletes the Accounts.txt before compiling and running)
- make freshp (deletes the Profiles.txt before compiling and running)
- make freshj (deletes the Jobs.txt before compiling and running)
- make freshm (deletes the Messages.txt before compiling and running)
- make freshc (deletes the Connections.txt and PendingRequests.txt before compiling and running)
- make freshap (deletes the Applications.txt and ApplicationsReport.txt before compiling and running)
- make fresh (deletes all produced files before compiling and running)
- make fresh (deletes all produced files before compiling and running)