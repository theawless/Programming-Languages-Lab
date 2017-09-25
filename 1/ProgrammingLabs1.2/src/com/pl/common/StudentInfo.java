package com.pl.common;

import java.io.*;

/**
 * Data class for student.
 */
public class StudentInfo implements Serializable {
    public int rollNumber;
    public String name;
    public int seat;
    public byte[] image;

    /**
     * Serialize the object into a byte array.
     *
     * @param obj object to serialize
     * @return byte array
     * @throws IOException in case the stream cannot be opened
     */
    public static byte[] serialize(Object obj) throws IOException {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        ObjectOutputStream os = new ObjectOutputStream(out);
        os.writeObject(obj);
        return out.toByteArray();
    }

    /**
     * Deserialize the byte array into the object.
     *
     * @param data byte array
     * @return object
     * @throws IOException            in case the stream cannot be opened
     * @throws ClassNotFoundException in case the byte array is not deserializable
     */
    public static StudentInfo deserialize(byte[] data) throws IOException, ClassNotFoundException {
        ByteArrayInputStream in = new ByteArrayInputStream(data);
        ObjectInputStream is = new ObjectInputStream(in);
        return (StudentInfo) is.readObject();
    }

    /**
     * Determines if the student info is a dummy one.
     *
     * @return boolean
     */
    public boolean isDummy() {
        return rollNumber == -1;
    }
}
