package as_deterministic_client;

import java.io.ByteArrayOutputStream;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.ServerSocket;
import java.net.Socket;

public class TCPServer {

	private static final int[] ports = {7896};

	public static void main(String[] args) {
		for(int port : ports) {
			new Thread(new Runnable() {				
				@Override
				public void run() {
					try {
						ServerSocket listenSocket = new ServerSocket(port);
						while (true) {
							new Connection(listenSocket.accept(), new TorXakisInterface(port)).start();
						}
					} catch (IOException e) {
						System.out.println("Listen :" + e.getMessage());
					}
				}
			}).start();;
		}
	}
	
	private static class Connection extends Thread {
		   private InputStream in;
	       private OutputStream out;
	       private Socket clientSocket;
	       private String newLine = null;
	       private TorXakisInterface txi;
	       private Connection(Socket aClientSocket, TorXakisInterface txi) {
               try {
                       clientSocket = aClientSocket;
                       in = clientSocket.getInputStream();
                       out = clientSocket.getOutputStream();
                       this.txi = txi;
               } catch (IOException e) {
                       System.out.println("Connection:" + e.getMessage());
               }
       }

       public void run() {
    	   	   int p = -1;
        	   ByteArrayOutputStream ba = new ByteArrayOutputStream();
               try {
            	   while(true) {
            		   int c = in.read();
            		   while(c != -1) {
            			   ba.write(c);
            			   if(newLine == null && p != -1 && c == '\n') {
            				   if (p == '\r') {
            					   newLine = "\r\n";
            				   }else {
            					   newLine = "\n";
            				   }
            			   }
            			   p = c;
            			   if (c == '\n') break;
            			   c = in.read();
            		   }
            		   String s = new String(ba.toByteArray()).strip();
            		   String r = txi.processMessage(s);
            		   if(r != null) {
            			   out.write((r+newLine).getBytes());
            			   out.flush();
                		   ba.reset();
            		   }else {
            			   break;
            		   }
            	   }
               } catch (EOFException e) {
                       System.out.println("EOF:" + e.getMessage());
               } catch (IOException e) {
                       System.out.println("IO:" + e.getMessage());
               } finally {
                       try {
                    	   	   in.close();
                    	   	   out.close();
                               clientSocket.close();
                       } catch (IOException e) {
                               /* close failed */}
               }
       }

	}
}
