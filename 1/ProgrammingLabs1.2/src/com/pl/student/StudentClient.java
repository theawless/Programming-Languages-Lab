package com.pl.student;

import com.pl.common.Client;
import com.pl.common.Constants;
import com.pl.common.StudentInfo;

import javax.swing.*;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * Interface for status.
 */
interface HasStatus {
    /**
     * Sets status
     *
     * @param status status
     */
    void setStatus(String status);
}

/**
 * Student client for logging in.
 */
public class StudentClient extends Client {
    private final HasStatus hasStatus;
    private final StudentInfo studentInfo;

    /**
     * Constructor.
     *
     * @param hasStatus   object having status
     * @param studentInfo student info
     */
    StudentClient(HasStatus hasStatus, StudentInfo studentInfo) {
        this.hasStatus = hasStatus;
        this.studentInfo = studentInfo;
    }

    /**
     * Writes the student info to the output and reads the status from the server.
     *
     * @see Client#clientWork(InputStream, OutputStream);
     */
    @Override
    protected void clientWork(InputStream inputStream, OutputStream outputStream) {
        try {
            DataOutputStream writer = new DataOutputStream(outputStream);
            DataInputStream reader = new DataInputStream(inputStream);

            writer.writeUTF(Constants.STUDENT_TOKEN);
            writer.writeInt(studentInfo.rollNumber);
            writer.writeUTF(studentInfo.name);
            writer.writeInt(studentInfo.seat);

            String message = reader.readUTF();

            writer.close();
            reader.close();

            SwingUtilities.invokeLater(() -> hasStatus.setStatus(message));
        } catch (Exception e) {
            SwingUtilities.invokeLater(() -> hasStatus.setStatus("Failed"));
        }
    }
}

