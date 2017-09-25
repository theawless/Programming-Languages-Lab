package com.pl.teacher;

import com.pl.common.Client;
import com.pl.common.Constants;
import com.pl.common.StudentInfo;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * Client for Teacher side.
 */
public class TeacherClient extends Client {
    private int seat;
    private StudentInfo studentInfo;

    /**
     * Constructor.
     *
     * @param seat seat number
     */
    TeacherClient(int seat) {
        this.seat = seat;
    }

    /**
     * Gets the fetched student info.
     *
     * @return student info
     */
    StudentInfo getStudentInfo() {
        return studentInfo;
    }

    /**
     * Writes the seat in the socket and reads the returned student info.
     *
     * @see Client#clientWork(InputStream, OutputStream);
     */
    @Override
    protected void clientWork(InputStream inputStream, OutputStream outputStream) {
        try {
            DataOutputStream writer = new DataOutputStream(outputStream);
            DataInputStream reader = new DataInputStream(inputStream);

            writer.writeUTF(Constants.TEACHER_TOKEN);
            writer.writeInt(seat);

            int count = reader.readInt();
            byte[] bytes = new byte[count];
            reader.readFully(bytes);

            studentInfo = StudentInfo.deserialize(bytes);

            writer.close();
            reader.close();
        } catch (Exception ignored) {
        }
    }
}

