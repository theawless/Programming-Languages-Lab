package com.pl;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

/**
 * Abstract implementation for threaded search.
 */
abstract class Searcher {
    static final int THREAD_COUNT = Runtime.getRuntime().availableProcessors(); // 4;
    private final List<Path> filePaths;

    /**
     * Constructor.
     *
     * @param filePaths paths of the files
     */
    Searcher(List<Path> filePaths) {
        this.filePaths = filePaths;
    }

    /**
     * Boilerplate for searching, creates the tasks and submits them to threaded search, and then fetches the results.
     *
     * @param tokens tokens to be searched for
     * @return search results
     */
    List<SearchFileTask.Result> search(List<String> tokens) {
        List<SearchFileTask> searchFileTasks = new ArrayList<>();
        for (Path filePath : filePaths) {
            SearchFileTask searchFileTask = new SearchFileTask(filePath, tokens);
            searchFileTasks.add(searchFileTask);
        }
        threadedSearch(searchFileTasks);
        List<SearchFileTask.Result> searchResults = new ArrayList<>();
        for (SearchFileTask searchFileTask : searchFileTasks) {
            searchResults.add(searchFileTask.getResult());
        }
        return searchResults;
    }

    /**
     * Abstract for threaded search.
     *
     * @param searchFileTasks search file tasks
     */
    protected abstract void threadedSearch(List<SearchFileTask> searchFileTasks);

    /**
     * Abstract for shutting down.
     */
    abstract void shutdown();
}

/**
 * Uses concurrent package for searching.
 */
class LibrarySearcher extends Searcher {
    private final ExecutorService executor;

    /**
     * Initialize executor service.
     *
     * @see Searcher#Searcher(List);
     */
    LibrarySearcher(List<Path> filePaths) {
        super(filePaths);
        executor = Executors.newFixedThreadPool(THREAD_COUNT);
    }

    /**
     * Uses executor service with a fixed thread pool to complete the tasks.
     *
     * @see Searcher#threadedSearch(List);
     */
    @Override
    protected void threadedSearch(List<SearchFileTask> searchFileTasks) {
        List<Future<?>> futures = new ArrayList<>();
        for (SearchFileTask searchFileTask : searchFileTasks) {
            executor.execute(searchFileTask);
            Future<?> future = executor.submit(searchFileTask);
            futures.add(future);
        }
        for (Future<?> future : futures) {
            try {
                future.get();
            } catch (InterruptedException | ExecutionException e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * Shuts down the executor service.
     *
     * @see Searcher#shutdown();
     */
    @Override
    void shutdown() {
        executor.shutdown();
    }
}

/**
 * Uses bare threads for searching.
 */
class ManualSearcher extends Searcher {
    private final static SearchFileTask POISON_PILL = null;

    /**
     * @see Searcher#Searcher(List);
     */
    ManualSearcher(List<Path> filePaths) {
        super(filePaths);
    }

    /**
     * Uses runnable, threads and a blocking enqueue for the search operation.
     * The threads will run continuously till the enqueue is has a null value which
     * is the poison pill for this queue. The thread will stop, and reinsert the null
     * value so other threads can stop too.
     *
     * @see Searcher#search(List);
     */
    @Override
    protected void threadedSearch(List<SearchFileTask> searchFileTasks) {
        CustomBlockingQueue<SearchFileTask> searchFilesQueue = new CustomBlockingQueue<>();
        List<Thread> threads = new ArrayList<>();
        for (int i = 0; i < THREAD_COUNT; ++i) {
            Runnable customRunnable = () -> {
                SearchFileTask searchFileTask;
                try {
                    while ((searchFileTask = searchFilesQueue.dequeue()) != null) {
                        searchFileTask.run();
                    }
                    // Reinsert poison pill.
                    searchFilesQueue.enqueue(POISON_PILL);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            };
            Thread thread = new Thread(customRunnable);
            threads.add(thread);
            thread.start();
        }
        for (SearchFileTask searchFileTask : searchFileTasks) {
            try {
                searchFilesQueue.enqueue(searchFileTask);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        // Insert a poison pill.
        try {
            searchFilesQueue.enqueue(POISON_PILL);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        for (Thread thread : threads) {
            try {
                thread.join();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * @see Searcher#shutdown();
     */
    @Override
    void shutdown() {
        // Nothing to shut down.
    }

    /**
     * Queue with a thread safe enqueue and dequeue operation.
     *
     * @param <T> type of enqueue elements
     */
    private class CustomBlockingQueue<T> {
        private static final int SIZE_LIMIT = Integer.MAX_VALUE;

        private Queue<T> queue = new LinkedList<>();

        /**
         * Synchronized enqueue operation.
         *
         * @param element element to add
         */
        synchronized void enqueue(T element) throws InterruptedException {
            while (isFull()) {
                wait();
            }
            boolean wasEmpty = isEmpty();
            queue.add(element);
            if (wasEmpty) {
                notifyAll();
            }
        }

        /**
         * Synchronized dequeue operation.
         *
         * @return element or null if empty queue
         */
        synchronized T dequeue() throws InterruptedException {
            while (isEmpty()) {
                wait();
            }
            boolean wasFull = isFull();
            T element = queue.poll();
            if (wasFull) {
                notifyAll();
            }
            return element;
        }

        /**
         * @return whether queue is empty
         */
        private boolean isEmpty() {
            return queue.size() == 0;
        }

        /**
         * @return whether queue is full
         */
        private boolean isFull() {
            return queue.size() == SIZE_LIMIT;
        }
    }
}