package com.pl.server;

import com.pl.common.Constants;
import com.pl.common.StudentInfo;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.Socket;

/**
 * Interface that defines the handler.
 */
interface ClientHandler {
    void handle();
}

/**
 * Handler for teacher client.
 */
class TeacherHandler implements ClientHandler {
    private final DataInputStream reader;
    private final DataOutputStream writer;

    /**
     * Constructor.
     *
     * @param reader input stream
     * @param writer output stream
     */
    TeacherHandler(DataInputStream reader, DataOutputStream writer) {
        this.reader = reader;
        this.writer = writer;
    }

    /**
     * Handle the teacher client.
     *
     * @see ClientHandler#handle();
     */
    @Override
    public void handle() {
        int seat = input();
        if (seat != -1) {
            StudentInfo studentInfo = process(seat);
            output(studentInfo);
        }
    }

    /**
     * Writes the student info to the output stream.
     *
     * @param studentInfo student info
     */
    private void output(StudentInfo studentInfo) {
        try {
            byte[] bytes = StudentInfo.serialize(studentInfo);
            writer.writeInt(bytes.length);
            writer.write(bytes);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Gets the student info for a seat.
     *
     * @param seat seat number
     * @return student info or dummy info
     */
    private StudentInfo process(int seat) {
        try {
            return DatabaseUtils.getStudentInfo(seat);
        } catch (Exception e) {
            StudentInfo dummy = new StudentInfo();
            dummy.rollNumber = -1;
            return dummy;
        }
    }

    /**
     * Get the seat number from the input stream.
     *
     * @return seat number
     */
    private int input() {
        int seat = -1;
        try {
            seat = reader.readInt();
        } catch (Exception ignored) {
        }
        return seat;
    }
}

/**
 * Handler for student client.
 */
class StudentHandler implements ClientHandler {
    private final DataInputStream reader;
    private final DataOutputStream writer;

    StudentHandler(DataInputStream reader, DataOutputStream writer) {
        this.reader = reader;
        this.writer = writer;
    }

    @Override
    public void handle() {
        StudentInfo studentInfo = input();
        String message = "Failed";
        if (studentInfo != null) {
            message = process(studentInfo);
        }
        output(message);
    }

    private String process(StudentInfo studentInfo) {
        try {
            return DatabaseUtils.updateSeat(studentInfo);
        } catch (Exception e) {
            return "Failed";
        }
    }

    private void output(String message) {
        try {
            writer.writeUTF(message);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private StudentInfo input() {
        try {
            StudentInfo studentInfo = new StudentInfo();
            studentInfo.rollNumber = reader.readInt();
            studentInfo.name = reader.readUTF();
            studentInfo.seat = reader.readInt();

            return studentInfo;
        } catch (IOException e) {
            return null;
        }
    }
}

/**
 * WorkerTask handles the client using client handler.
 */
class WorkerTask implements Runnable {
    private final Socket clientSocket;

    /**
     * Constructor.
     *
     * @param clientSocket client socket
     */
    WorkerTask(Socket clientSocket) {
        this.clientSocket = clientSocket;
    }

    /**
     * @see Runnable#run();
     */
    @Override
    public void run() {
        try {
            delegateHandle();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Decides the client handler and runs it.
     * Token (the first string) is used for deciding.
     *
     * @throws IOException in case socket streams could not be opened.
     */
    private void delegateHandle() throws IOException {
        DataInputStream reader = new DataInputStream(clientSocket.getInputStream());
        DataOutputStream writer = new DataOutputStream(clientSocket.getOutputStream());
        String header = reader.readUTF();

        ClientHandler handler = null;
        switch (header) {
            case Constants.TEACHER_TOKEN:
                handler = new TeacherHandler(reader, writer);
                break;
            case Constants.STUDENT_TOKEN:
                handler = new StudentHandler(reader, writer);
                break;
        }
        if (handler != null) {
            handler.handle();
        }
        writer.close();
        reader.close();
    }
}