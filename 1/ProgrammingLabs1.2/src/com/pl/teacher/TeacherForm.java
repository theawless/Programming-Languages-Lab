package com.pl.teacher;

import javax.swing.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

/**
 * Teacher GUI.
 */
class TeacherForm extends JDialog {
    private JButton buttonRefresh;
    private JButton buttonClose;
    private LayoutMaker layoutMaker;
    private JPanel contentPane;
    private JPanel gridPaneContainer;

    /**
     * Constructor.
     */
    private TeacherForm() {
        setContentPane(contentPane);
        setModal(true);
        getRootPane().setDefaultButton(buttonRefresh);

        buttonRefresh.addActionListener(e -> onRefresh());
        buttonClose.addActionListener(e -> onClose());

        setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                onClose();
            }
        });

        Executors.newScheduledThreadPool(1)
                .scheduleAtFixedRate(() -> SwingUtilities.invokeLater(this::onRefresh), 5, 1, TimeUnit.SECONDS);
    }

    /**
     * Sets the layout maker.
     *
     * @param layoutMaker layout maker
     */
    private void setLayoutMaker(LayoutMaker layoutMaker) {
        this.layoutMaker = layoutMaker;
        JScrollPane scrollPane = new JScrollPane(layoutMaker.getGridPane(),
                JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
                JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
        gridPaneContainer.add(scrollPane);
    }

    /**
     * Listens for refresh button.
     */
    private void onRefresh() {
        layoutMaker.update();
    }

    /**
     * Listens for close button.
     */
    private void onClose() {
        dispose();
        layoutMaker.shutdown();
    }

    /**
     * Sets theme.
     */
    private static void setLookAndFeel() {
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        } catch (Exception ignored) {
        }
    }

    /**
     * Entry point of the program.
     * Select the layout maker based on program arguments.
     *
     * @param args program arguments
     */
    public static void main(String[] args) {
        setLookAndFeel();

        TeacherForm dialog = new TeacherForm();
        LayoutMaker layoutMaker;

        if (args.length == 0 || Integer.parseInt(args[0]) == 1) {
            layoutMaker = new UnThreadedLayoutMaker();
        } else if (Integer.parseInt(args[0]) == 2) {
            layoutMaker = new ThreadedLayoutMaker();
        } else {
            layoutMaker = new ForkJoinLayoutMaker();
        }
        System.out.println("Using " + layoutMaker.getClass().getSimpleName());
        dialog.setLayoutMaker(layoutMaker);

        dialog.pack();
        dialog.setVisible(true);
        System.exit(0);
    }
}
