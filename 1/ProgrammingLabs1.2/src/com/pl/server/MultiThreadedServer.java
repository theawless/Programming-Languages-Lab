package com.pl.server;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import static com.pl.common.Constants.SERVER_PORT;

/**
 * Multi threaded server.
 */
class MultiThreadedServer {
    private final static int THREAD_COUNT = Runtime.getRuntime().availableProcessors();
    private ServerSocket serverSocket;
    private boolean hasStopped = false;
    private ExecutorService executor;

    /**
     * Constructor.
     * Start executor.
     */
    MultiThreadedServer() {
        executor = Executors.newFixedThreadPool(THREAD_COUNT);
    }

    /**
     * Opens socket.
     */
    private void openSocket() {
        try {
            serverSocket = new ServerSocket(SERVER_PORT);
        } catch (IOException e) {
            throw new RuntimeException("Not able to open port: " + SERVER_PORT, e);
        }
    }

    /**
     * Starts the server.
     */
    void start() {
        openSocket();

        while (!hasStopped) {
            Socket clientSocket;
            try {
                // Accept the client and submit it to the executor service.
                clientSocket = serverSocket.accept();
            } catch (IOException e) {
                if (hasStopped) {
                    return;
                }
                throw new RuntimeException("StudentClient cannot be connected", e);
            }
            executor.execute(new WorkerTask(clientSocket));
        }
    }

    /**
     * Stops the server.
     */
    void stop() {
        hasStopped = true;
        try {
            serverSocket.close();
        } catch (IOException e) {
            throw new RuntimeException("Server can not be closed", e);
        }
        executor.shutdown();
        try {
            executor.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}
