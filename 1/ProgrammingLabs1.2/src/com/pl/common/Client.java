package com.pl.common;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;

import static com.pl.common.Constants.SERVER_PORT;

/**
 * Abstract class for a Socket Client.
 */
public abstract class Client implements Runnable {
    private Socket clientSocket;

    /**
     * Opens the socket.
     */
    private void openSocket() {
        try {
            clientSocket = new Socket(Constants.SERVER_ADDRESS, SERVER_PORT);
        } catch (IOException e) {
            throw new RuntimeException("Not able to open port: " + SERVER_PORT, e);
        }
    }

    /**
     * Opens the socket, performs the client work and closes the socket.
     *
     * @see Runnable#run();
     */
    @Override
    public void run() {
        openSocket();
        try {
            clientWork(clientSocket.getInputStream(), clientSocket.getOutputStream());
        } catch (IOException e) {
            e.printStackTrace();
        }
        closeSocket();
    }

    /**
     * Inheritors will perform their work here.
     * They also need to close the given streams.
     *
     * @param inputStream  input stream of the socket
     * @param outputStream output stream of the socket
     */
    protected abstract void clientWork(InputStream inputStream, OutputStream outputStream);

    /**
     * Closes the socket.
     */
    private void closeSocket() {
        try {
            clientSocket.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
