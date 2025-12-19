default: InCollege.cob
	cobc -x -Wall InCollege.cob
	./InCollege

compile: InCollege.cob
	cobc -x -Wall InCollege.cob

fresha:
	rm -f Accounts.txt
	cobc -x -Wall InCollege.cob
	./InCollege

freshp:
	rm -f Profiles.txt
	cobc -x -Wall InCollege.cob
	./InCollege

fresh:
	rm -f Accounts.txt
	rm -f Profiles.txt*
	rm -f PendingRequests.txt
	rm -f TempPending.txt
	rm -f Connections.txt
	rm -f Jobs.txt
	rm -f Applications.txt
	rm -f ApplicationsReport.txt
	rm -f Messages.txt
	cobc -x -Wall InCollege.cob
	./InCollege

cleanstorage:
	rm -f Accounts.txt
	rm -f Profiles.txt*
	rm -f PendingRequests.txt
	rm -f TempPending.txt
	rm -f Connections.txt
	rm -f Jobs.txt
	rm -f Applications.txt
	rm -f ApplicationsReport.txt
	rm -f Messages.txt

clean:
	rm -f InCollege
	rm -f Accounts.txt
	rm -f Profiles.txt*
	rm -f PendingRequests.txt
	rm -f TempPending.txt
	rm -f Connections.txt
	rm -f Jobs.txt
	rm -f Applications.txt
	rm -f ApplicationsReport.txt
	rm -f Messages.txt