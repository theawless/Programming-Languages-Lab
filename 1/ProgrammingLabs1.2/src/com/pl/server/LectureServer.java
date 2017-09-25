package com.pl.server;

/**
 * Handles the lecture server.
 */
public class LectureServer {
    private static MultiThreadedServer server;

    /**
     * Entry point of the program.
     *
     * @param args program arguments
     */
    public static void main(String[] args) {
        server = new MultiThreadedServer();
        Runtime.getRuntime().addShutdownHook(new Thread(LectureServer::stop));
        start();
    }

    /**
     * Starts the server.
     */
    private static void start() {
        server.start();
    }

    /**
     * Stops the server.
     */
    private static void stop() {
        server.stop();
        try {
            DatabaseUtils.removeAllSeats();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}