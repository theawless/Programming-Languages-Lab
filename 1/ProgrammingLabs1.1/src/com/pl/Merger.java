package com.pl;

import java.lang.reflect.Array;
import java.util.List;
import java.util.concurrent.ForkJoinPool;

/**
 * Merges the search results using fork join.
 */
class Merger {
    private static final int THREAD_COUNT = Runtime.getRuntime().availableProcessors(); // 4
    private final ForkJoinPool forkJoinPool;

    Merger() {
        forkJoinPool = new ForkJoinPool(THREAD_COUNT);
    }

    /**
     * Merge operation.
     *
     * @param unsortedResultsList unsorted search results
     * @return sorted search results.
     */
    SearchFileTask.Result[] merge(List<SearchFileTask.Result> unsortedResultsList) {
        SearchFileTask.Result[] sortedResults = new SearchFileTask.Result[unsortedResultsList.size()];
        unsortedResultsList.toArray(sortedResults);
        if (sortedResults.length > 1) {
            sort(sortedResults);
        }
        return sortedResults;
    }

    /**
     * Sort operation that uses fork join pool.
     *
     * @param array array to be sorted
     * @param <T>   type of the array
     */
    private <T extends Comparable<? super T>> void sort(T[] array) {
        T[] helper = (T[]) Array.newInstance(array[0].getClass(), array.length);
        forkJoinPool.invoke(new MergeSortTask<>(array, helper, 0, array.length - 1));
    }

    /**
     * Shutdown fork join pool.
     */
    void shutdown() {
        forkJoinPool.shutdown();
    }
}