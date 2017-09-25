package com.pl.teacher;

import com.pl.common.StudentInfo;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;

/**
 * Abstract class for layout maker.
 * Contains functionality for making gui, making clients and loading images.
 */
abstract class LayoutMaker {
    static final int ROWS = 5;
    static final int COLUMNS = 12;
    private final JPanel gridPane;
    private final List<StudentIcon> studentIcons;

    /**
     * Constructor.
     * Make and add the grid to the pane.
     */
    LayoutMaker() {
        gridPane = new JPanel(new GridLayout(ROWS, COLUMNS));
        gridPane.setAutoscrolls(true);
        gridPane.setSize(5000, 500);
        studentIcons = new ArrayList<>();
        for (int i = 0; i < ROWS * COLUMNS; ++i) {
            StudentIcon studentIcon = new StudentIcon();
            gridPane.add(studentIcon.getPanel());
            studentIcons.add(studentIcon);
            int seat = i;
            SwingUtilities.invokeLater(() -> studentIcon.setSeatInfo(seat));
        }
    }

    /**
     * Gets the grid pane.
     *
     * @return grid pane
     */
    JPanel getGridPane() {
        return gridPane;
    }

    /**
     * Update the layout.
     */
    void update() {
        updateStudentIcons();
    }

    /**
     * Update the student icons.
     * Implemented by inheritors.
     */
    abstract void updateStudentIcons();

    /**
     * Calls the GUI to update the student icon.
     * This function also scales the student image.
     *
     * @param studentInfo student info
     * @param seat        seat number
     */
    void updateStudentIcon(StudentInfo studentInfo, int seat) {
        StudentIcon studentIcon = studentIcons.get(seat);
        if (studentInfo == null) {
            SwingUtilities.invokeLater(studentIcon::clear);
            return;
        }
        ImageIcon unscaledImageIcon = new ImageIcon(studentInfo.image);
        ImageIcon studentImage = new ImageIcon(unscaledImageIcon.getImage().getScaledInstance(50, 50, Image.SCALE_DEFAULT));
        SwingUtilities.invokeLater(() -> studentIcon.setStudentInfo(studentInfo, studentImage));
    }

    /**
     * Creates a teacher client and fetches the student info.
     *
     * @param seat seat number
     * @return student info
     */
    StudentInfo getStudentInfo(int seat) {
        TeacherClient teacherClient = new TeacherClient(seat);
        teacherClient.run();
        StudentInfo studentInfo = teacherClient.getStudentInfo();
        if (studentInfo.isDummy()) {
            return null;
        }
        return studentInfo;
    }

    /**
     * Shuts down.
     */
    abstract void shutdown();
}
