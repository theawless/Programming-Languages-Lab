package com.pl;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Loads a file and searches it for given tokens.
 */
class SearchFileTask implements Runnable {
    private Result result;
    private List<String> tokens;
    /**
     * Constructor.
     *
     * @param filePath file to be searched
     * @param tokens   tokens to be searched for
     */
    SearchFileTask(Path filePath, List<String> tokens) {
        this.tokens = tokens;
        result = new Result();
        result.filePath = filePath;
        result.tokenIndexes = new ArrayList<>(Collections.nCopies(tokens.size(), -1));
    }

    /**
     * Gets the words from the given file.
     *
     * @param filePath file to be read
     * @return words
     */
    private static List<String> getWords(Path filePath) {
        try {
            return Files.readAllLines(filePath).stream()
                    .map(l -> l.split("\\s+"))
                    .flatMap(Arrays::stream)
                    .map(w -> w.replaceAll("[^a-zA-Z0-9]", ""))
                    .map(String::toLowerCase)
                    .collect(Collectors.toList());
        } catch (IOException e) {
            e.printStackTrace();
        }
        return Collections.emptyList();
    }

    /**
     * Searches the tokens in the words given taking into account their relative positioning.
     *
     * @param words words to search from
     */
    private void search(List<String> words) {
        int minDistance = Integer.MAX_VALUE, startTokenIndex = 0;

        while (startTokenIndex < words.size()) {
            List<Integer> tokenIndexes = new ArrayList<>(Collections.nCopies(tokens.size(), -1));
            int subListTokenIndex = words.subList(startTokenIndex, words.size()).indexOf(tokens.get(0));
            int newTokenIndex = subListTokenIndex + startTokenIndex;
            tokenIndexes.set(0, newTokenIndex);
            if (subListTokenIndex == -1) {
                break;
            }
            for (int i = 1; i < tokens.size(); ++i) {
                subListTokenIndex = words.subList(newTokenIndex + 1, words.size()).indexOf(tokens.get(i));
                if (subListTokenIndex == -1) {
                    break;
                }
                newTokenIndex = subListTokenIndex + newTokenIndex + 1;
                tokenIndexes.set(i, newTokenIndex);
            }
            if (subListTokenIndex == -1) {
                break;
            }
            int minDistanceCandidate = tokenIndexes.get(tokenIndexes.size() - 1) - tokenIndexes.get(0);
            if (minDistanceCandidate < minDistance) {
                minDistance = minDistanceCandidate;
                result.tokenIndexes = tokenIndexes;
            }
            startTokenIndex = tokenIndexes.get(0) + 1;
        }
    }

    /**
     * Gets the search result.
     *
     * @return the search result
     */
    Result getResult() {
        return result;
    }

    /**
     * Just calls the search function.
     *
     * @see Runnable#run();
     */
    @Override
    public void run() {
        search(getWords(result.filePath));
    }

    /**
     * Describes the indexes of the tokens in a file.
     */
    class Result implements Comparable {
        Path filePath;
        List<Integer> tokenIndexes;

        /**
         * Compares the relative position of the result.
         *
         * @see Comparable#compareTo(Object);
         */
        @Override
        public int compareTo(Object o) {
            if (o instanceof Result) {
                Result result = (Result) o;
                int distance1 = tokenIndexes.get(tokenIndexes.size() - 1) - tokenIndexes.get(0);
                int distance2 = result.tokenIndexes.get(result.tokenIndexes.size() - 1) - result.tokenIndexes.get(0);
                return distance1 - distance2;
            } else {
                throw new IllegalArgumentException();
            }
        }
    }
}