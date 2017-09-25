package com.pl;

import java.util.concurrent.RecursiveAction;

/**
 * Recursive task of the fork join.
 *
 * @param <T> type of array
 */
class MergeSortTask<T extends Comparable<? super T>> extends RecursiveAction {
    private final T[] array;
    private final T[] helper;
    private final int low;
    private final int high;

    /**
     * Constructor.
     *
     * @param array  array to be sorted
     * @param helper helper array, used for merge operation
     * @param low    lower index of array part in consideration
     * @param high   higher index of array part in consideration
     */
    MergeSortTask(T[] array, T[] helper, int low, int high) {
        this.array = array;
        this.helper = helper;
        this.low = low;
        this.high = high;
    }

    /**
     * Recursively calls the merge sort task on the two halves of the array.
     *
     * @see MergeSortTask#compute();
     */
    @Override
    protected void compute() {
        if (low >= high) return;
        int mid = low + (high - low) / 2;
        MergeSortTask<T> left = new MergeSortTask<>(array, helper, low, mid);
        MergeSortTask<T> right = new MergeSortTask<>(array, helper, mid + 1, high);
        invokeAll(left, right);
        merge(this.array, this.helper, this.low, mid, this.high);
    }

    /**
     * Merge operation using single helper array.
     * Left and right sides of the helper array are used to merge.
     *
     * @param array  array to be sorted
     * @param helper helper array, used for merge operation
     * @param low    lower index of array part in consideration
     * @param mid    mid index of array part in consideration
     * @param high   higher index of array part in consideration
     */
    private void merge(T[] array, T[] helper, int low, int mid, int high) {
        System.arraycopy(array, low, helper, low, high + 1 - low);
        int i = low, j = mid + 1;
        for (int k = low; k <= high; k++) {
            if (i > mid) {
                array[k] = helper[j++];
            } else if (j > high) {
                array[k] = helper[i++];
            } else if (isLess(helper[i], helper[j])) {
                array[k] = helper[i++];
            } else {
                array[k] = helper[j++];
            }
        }
    }

    /**
     * Check if first object is lesser than second object
     *
     * @param a first object
     * @param b second object
     * @return bool
     */
    private boolean isLess(T a, T b) {
        return a.compareTo(b) < 0;
    }
}