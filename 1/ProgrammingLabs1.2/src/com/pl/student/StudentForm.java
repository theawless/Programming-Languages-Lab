package com.pl.student;

import com.pl.common.Client;
import com.pl.common.StudentInfo;

import javax.swing.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * Student login GUI.
 */
public class StudentForm extends JDialog implements HasStatus {
    private JPanel contentPane;
    private JButton buttonOK;
    private JButton buttonCancel;
    private JTextField textFieldRollNumber;
    private JTextField textFieldName;
    private JLabel labelStatus;
    private JTextField textFieldSeat;
    private ExecutorService executor;

    /**
     * Constructor.
     * Start executor service.
     */
    private StudentForm() {
        setContentPane(contentPane);
        setModal(true);
        getRootPane().setDefaultButton(buttonOK);

        buttonOK.addActionListener(e -> onOK());
        buttonCancel.addActionListener(e -> onCancel());
        setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                onCancel();
            }
        });

        executor = Executors.newSingleThreadExecutor();
    }

    /**
     * Listens on OK button.
     * <p>
     * Validate the data and then start the client to communicate with server.
     */
    private void onOK() {
        String rollNumber = textFieldRollNumber.getText();
        String name = textFieldName.getText();
        String seat = textFieldSeat.getText();

        StudentInfo studentInfo = new StudentInfo();
        rollNumber = rollNumber.replaceAll("[^0-9]", "");
        name = name.replaceAll("[^a-zA-Z ]", "").toLowerCase();
        seat = seat.replaceAll("[^0-9]", "");

        if (rollNumber.isEmpty()) {
            setStatus("Check roll number");
            return;
        }
        if (name.isEmpty()) {
            setStatus("Check name");
            return;
        }
        if (seat.isEmpty()) {
            setStatus("Check roll number");
            return;
        }

        studentInfo.rollNumber = Integer.parseInt(rollNumber);
        studentInfo.name = name;
        studentInfo.seat = Integer.parseInt(seat);

        Client studentClient = new StudentClient(this, studentInfo);
        executor.execute(studentClient);
    }

    /**
     * Listens on cancel button.
     */
    private void onCancel() {
        executor.shutdown();
        dispose();
    }

    /**
     * Sets the theme.
     */
    private static void setLookAndFeel() {
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        } catch (Exception ignored) {
        }
    }

    /**
     * Entry point of the program.
     *
     * @param args program arguments
     */
    public static void main(String[] args) {
        setLookAndFeel();
        StudentForm dialog = new StudentForm();
        dialog.pack();
        dialog.setVisible(true);
        System.exit(0);
    }

    /**
     * Show the status in the GUI.
     *
     * @see HasStatus#setStatus(String);
     */
    @Override
    public void setStatus(String status) {
        labelStatus.setText(status);
    }
}
