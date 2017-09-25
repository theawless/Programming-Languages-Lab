package com.pl.teacher;

import com.pl.common.StudentInfo;

import javax.swing.*;

/**
 * Student Icon GUI.
 */
public class StudentIcon {
    private JPanel panelStudent;
    private JLabel labelSeat;
    private JLabel labelImage;
    private JLabel labelRollNumber;
    private JLabel labelName;

    /**
     * Set seat info on the GUI.
     *
     * @param seat seat info
     */
    void setSeatInfo(int seat) {
        labelSeat.setText(String.valueOf(seat));
    }

    /**
     * Display student info on the GUI.
     *
     * @param studentInfo  student info
     * @param studentImage student image
     */
    void setStudentInfo(StudentInfo studentInfo, Icon studentImage) {
        labelRollNumber.setText(String.valueOf(studentInfo.rollNumber));
        labelName.setText(studentInfo.name);
        labelImage.setIcon(studentImage);
        labelImage.setText("");
        panelStudent.revalidate();
    }

    /**
     * Clear the student GUI.
     */
    void clear() {
        labelRollNumber.setText("-");
        labelName.setText("-");
        labelImage.setText("-");
        labelImage.setIcon(null);
    }

    JPanel getPanel() {
        return panelStudent;
    }

    /**
     * Java constructs the swing GUI using this.
     *
     * @param args program arguments
     */
    public static void main(String[] args) {
        JFrame frame = new JFrame("StudentIcon");
        frame.setContentPane(new StudentIcon().panelStudent);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.pack();
        frame.setVisible(true);
    }
}
