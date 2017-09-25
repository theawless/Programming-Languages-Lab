package com.pl.teacher;

import com.pl.common.StudentInfo;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.RecursiveAction;

/**
 * Doesn't use threads for making layout.
 */
class UnThreadedLayoutMaker extends LayoutMaker {
    /**
     * @see LayoutMaker#updateStudentIcons();
     */
    @Override
    void updateStudentIcons() {
        for (int i = 0; i < ROWS; ++i) {
            for (int j = 0; j < COLUMNS; ++j) {
                int seat = i * COLUMNS + j;
                StudentInfo studentInfo = getStudentInfo(seat);
                updateStudentIcon(studentInfo, seat);
            }
        }
    }

    /**
     * Nothing to shutdown.
     *
     * @see LayoutMaker#shutdown();
     */
    @Override
    void shutdown() {
    }
}

/**
 * Use executor service to make layout.
 */
class ThreadedLayoutMaker extends LayoutMaker {
    private static final int THREAD_COUNT = Runtime.getRuntime().availableProcessors();
    private final ExecutorService executor;

    /**
     * Constructor.
     * Initialise executor service.
     */
    ThreadedLayoutMaker() {
        executor = Executors.newFixedThreadPool(THREAD_COUNT);
    }

    /**
     * @see LayoutMaker#updateStudentIcons();
     */
    @Override
    void updateStudentIcons() {
        for (int i = 0; i < ROWS; ++i) {
            for (int j = 0; j < COLUMNS; ++j) {
                final int seat = i * COLUMNS + j;
                executor.execute(() -> {
                    StudentInfo studentInfo = getStudentInfo(seat);
                    updateStudentIcon(studentInfo, seat);
                });
            }
        }
    }

    /**
     * Shut down executor.
     *
     * @see LayoutMaker#shutdown();
     */
    @Override
    void shutdown() {
        executor.shutdown();
    }
}

class ForkJoinLayoutMaker extends LayoutMaker {
    private static final int THREAD_COUNT = Runtime.getRuntime().availableProcessors();
    private final ForkJoinPool forkJoinPool;

    /**
     * Constructor.
     * Initialise fork join pool.
     */
    ForkJoinLayoutMaker() {
        forkJoinPool = new ForkJoinPool(THREAD_COUNT);
    }

    /**
     * @see LayoutMaker#updateStudentIcons();
     */
    @Override
    void updateStudentIcons() {
        forkJoinPool.execute(new RowTask());
    }

    /**
     * @see LayoutMaker#shutdown();
     */
    @Override
    void shutdown() {
        forkJoinPool.shutdown();
    }

    /**
     * Divide the row into columns.
     */
    class RowTask extends RecursiveAction {

        /**
         * @see RecursiveAction#compute();
         */
        @Override
        protected void compute() {
            List<ColumnTask> columnTasks = new ArrayList<>();
            for (int row = 0; row < ROWS; ++row) {
                columnTasks.add(new ColumnTask(row));
            }
            invokeAll(columnTasks);
        }
    }

    /**
     * Divide the column into points.
     */
    class ColumnTask extends RecursiveAction {
        private final int row;

        /**
         * Constructor.
         *
         * @param row row in consideration
         */
        ColumnTask(int row) {
            this.row = row;
        }

        /**
         * @see RecursiveAction#compute();
         */
        @Override
        protected void compute() {
            List<SeatTask> seatTasks = new ArrayList<>();
            for (int column = 0; column < COLUMNS; ++column) {
                seatTasks.add(new SeatTask(row, column));
            }
            invokeAll(seatTasks);
        }
    }

    /**
     * Fetches student info individually and update GUI.
     */
    class SeatTask extends RecursiveAction {
        private int row;
        private int column;

        /**
         * Constructor.
         *
         * @param row    row in consideration
         * @param column column in consideration
         */
        SeatTask(int row, int column) {
            this.row = row;
            this.column = column;
        }

        /**
         * @see RecursiveAction#compute();
         */
        @Override
        protected void compute() {
            final int seat = row * COLUMNS + column;
            StudentInfo studentInfo = getStudentInfo(seat);
            updateStudentIcon(studentInfo, seat);
        }
    }
}