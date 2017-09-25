package com.pl.server;

import com.pl.common.StudentInfo;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.*;

/**
 * Provides operations on the database.
 */
class DatabaseUtils {
    static {
        // Get the driver before the first access to this class.
        try {
            Class.forName("com.mysql.jdbc.Driver");
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
    }

    /**
     * Gets the sql connection.
     *
     * @return sql connection
     */
    private static Connection getSqlConnection() {
        try {
            return DriverManager.getConnection("jdbc:mysql://localhost:3306/pl", "root", "root");
        } catch (SQLException e) {
            e.printStackTrace();
        }
        return null;
    }

    /**
     * Updates the seat corresponding to given student.
     *
     * @param studentInfo student info
     * @return success message of the operation
     * @throws Exception in case there is some database error
     */
    static String updateSeat(StudentInfo studentInfo) throws Exception {
        final String query1 = "SELECT * FROM pl.class_info WHERE seat = ?;";
        try (Connection connection = getSqlConnection();
             PreparedStatement statement = connection != null ? connection.prepareStatement(query1) : null) {
            statement.setInt(1, studentInfo.seat);
            try (ResultSet resultSet = statement.executeQuery()) {
                if (resultSet.next()) {
                    return "Seat already taken";
                }
            }
        }

        final String query2 = "UPDATE pl.class_info SET seat = ? WHERE roll_number = ? AND name = ?;";
        try (Connection connection = getSqlConnection();
             PreparedStatement statement = connection != null ? connection.prepareStatement(query2) : null) {
            statement.setInt(2, studentInfo.rollNumber);
            statement.setString(3, studentInfo.name);
            statement.setInt(1, studentInfo.seat);
            if (statement.executeUpdate() == 0) {
                return "No student found with these details";
            }
        }
        return "Success";
    }

    /**
     * Resets the database for a new lecture.
     *
     * @throws Exception in case of a database error.
     */
    static void removeAllSeats() throws Exception {
        final String query = "UPDATE pl.class_info SET seat = ?;";
        try (Connection connection = getSqlConnection();
             PreparedStatement statement = connection != null ? connection.prepareStatement(query) : null) {
            statement.setInt(1, -1);
            statement.execute();
        }
    }

    /**
     * Fetches the student info given the seat.
     *
     * @param seat seat number
     * @return student info
     * @throws Exception if no student is found
     */
    static StudentInfo getStudentInfo(int seat) throws Exception {
        final String query = "SELECT * FROM pl.class_info WHERE seat = ?;";
        try (Connection connection = getSqlConnection();
             PreparedStatement statement = connection != null ? connection.prepareStatement(query) : null) {
            statement.setInt(1, seat);
            try (ResultSet resultSet = statement.executeQuery()) {
                if (resultSet.next()) {
                    StudentInfo studentInfo = new StudentInfo();
                    studentInfo.name = resultSet.getString(1);
                    studentInfo.rollNumber = resultSet.getInt(2);
                    studentInfo.seat = resultSet.getInt(3);
                    studentInfo.image = getStudentImage(studentInfo.rollNumber);
                    return studentInfo;
                }
                throw new IllegalArgumentException("No student found for this seat");
            }
        }
    }

    /**
     * Fetches the student image from the disk, given the roll number.
     *
     * @param rollNumber roll number of the student
     * @return image in byte array
     * @throws IOException if there is an error in loading from disk
     */
    private static byte[] getStudentImage(int rollNumber) throws IOException {
        Path path = Paths.get("images", String.valueOf(rollNumber) + ".jpg");
        return Files.readAllBytes(path);
    }
}
