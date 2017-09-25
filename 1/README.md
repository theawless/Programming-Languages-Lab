# Assignment 1

### Submission by
* Abhinav Singh 140101002
* Yash Pote 140101080

### Multiple File Search Utility
The program searches for the given query in the files using multiple threads. The result is then merged using the merge sort in a ForkJoin mechanism.

The program should be run with program arguments.
If program argument is present ManualSearcher will be chosen, otherwise LibrarySearcher.

* ManualSearcher - Uses bare Threads and a custom Blocking Queue implementation.
* LibrarySearcher - Uses concurrent package for searching (Executor Service).

### Classroom visualization
This program is divided into 3 parts.

* Server - Multi threaded server which handles both student and teacher.
* Student - GUI with client to fetch student seating data layout from server.
* Teacher - GUI with client to send student login data to server.

The images are stored on the server, and the server uses a mysql database to store student login info. All the student seating data (including images) is sent using sockets. Login data is also sent using sockets, and the server can report back the errors in the data back to the student client.

The layout generation can be done in 3 ways (depending on the program argument).
The layout has been attached with a spinner, which shows how active the GUI is (in case of heavy computations on the UI thread, the spinner will hang).

### Q. Also report your observational differences among the above three strategies.
* UnThreadedLayoutMaker - The spinner hanged, and the UI got
stuck.
* ThreadedLayoutMaker - The spinner didn’t hang.
* ForkJoinLayoutMaker - The spinner didn’t hang. No noticable difference was seen between Threaded and ForkJoin method.

### Q. Justify the number of threads creation in your program.
For searching the files a major computation task - we selected 4 threads (equal to the number of the processors). This is done so that maximum computation can be done parallely.

For merge sort (a computation task) - 4 threads same as above reasoning.

For layout generation (IO task - for fetching the images + computation - for scaling the images ) we selected 4 threads. Even though IO operations won’t benefit from 4 threads, but the scaling of the images do.
