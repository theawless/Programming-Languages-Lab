package com.pl;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * Gets input, calls for processing and prints output.
 */
class SearchProgram {
    private final Path outputPath;
    private final Merger merger;
    private Searcher searcher;

    /**
     * Constructor.
     *
     * @param outputPath path of the output file
     */
    SearchProgram(Path outputPath) {
        this.outputPath = outputPath;
        merger = new Merger();
    }

    /**
     * Sets which implementation of searcher should be used.
     *
     * @param searcher implementation
     */
    void setSearcher(Searcher searcher) {
        this.searcher = searcher;
    }

    /**
     * Takes user input and calls for processing after tokenising it.
     */
    void input() {
        Scanner scanner = new Scanner(System.in);
        do {
            System.out.printf("Enter your query- ");
            String line = scanner.nextLine();
            List<String> tokens = new ArrayList<>();
            for (String token : line.split("\\s+")) {
                tokens.add(token.replaceAll("[^a-zA-Z0-9]", "").toLowerCase());
            }

            process(tokens);
        } while (true);
    }

    /**
     * Calls searcher and merger.
     *
     * @param tokens tokens of the search query
     */
    private void process(List<String> tokens) {
        List<SearchFileTask.Result> searchResults = searcher.search(tokens);
        List<SearchFileTask.Result> filteredSearchResults = new ArrayList<>();
        for (SearchFileTask.Result result : searchResults) {
            if (!result.tokenIndexes.contains(-1)) {
                filteredSearchResults.add(result);
            }
        }
        SearchFileTask.Result[] finalSearchResults = merger.merge(filteredSearchResults);
        output(tokens, finalSearchResults);
    }

    /**
     * Prints the results on a file, and the console.
     *
     * @param tokens        tokens of the search results
     * @param searchResults search results to be printed
     */
    private void output(List<String> tokens, SearchFileTask.Result[] searchResults) {
        List<String> lines = new ArrayList<>();
        lines.add(String.join(" ", tokens) + " found in:");
        for (SearchFileTask.Result result : searchResults) {
            StringBuilder line = new StringBuilder(result.filePath.toString() + " -");
            for (int i = 0; i < result.tokenIndexes.size(); ++i) {
                line.append(" ").append(tokens.get(i)).append(":").append(result.tokenIndexes.get(i));
            }
            lines.add(line.toString());
        }
        lines.add("");
        for (String line : lines) {
            System.out.println(line);
        }
        try {
            if (!Files.isWritable(outputPath)) {
                Files.createFile(outputPath);
            }
            Files.write(outputPath, lines, StandardOpenOption.APPEND);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Shut down any thread resources.
     */
    void shutdown() {
        searcher.shutdown();
        merger.shutdown();
    }
}

/**
 * Handles program arguments and selects searchers.
 */
public class Main {
    private static final String FOLDER_PATH = "files";
    private static final String OUTPUT_FILE = "output";

    /**
     * Gets all the paths of files in the given folder.
     *
     * @param folderPath folder name
     * @return paths of files
     */
    private static List<Path> getFiles(String folderPath) {
        List<Path> filePaths = new ArrayList<>();
        File[] files = new File(folderPath).listFiles();
        if (files != null) {
            for (File file : files) {
                Path path = Paths.get(folderPath, file.getName());
                filePaths.add(path);
            }
        }
        return filePaths;
    }

    /**
     * Entry point of the program. If there are any program arguments, we will run
     * the ManualSearcher(bare threads) otherwise LibrarySearcher(concurrent package).
     *
     * @param args program arguments
     */
    public static void main(String[] args) {
        SearchProgram searchProgram = new SearchProgram(Paths.get(OUTPUT_FILE));
        Searcher searcher;
        if (args.length > 0) {
            searcher = new ManualSearcher(getFiles(FOLDER_PATH));
        } else {
            searcher = new LibrarySearcher(getFiles(FOLDER_PATH));
        }
        System.out.println("Using " + searcher.getClass().getSimpleName());
        searchProgram.setSearcher(searcher);
        Runtime.getRuntime().addShutdownHook(new Thread(searchProgram::shutdown));

        searchProgram.input();
    }
}