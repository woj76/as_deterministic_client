package as_deterministic_client;

public class TorXakisInterface {
	private int id;
	private DeterministicClient dc;
	
	public TorXakisInterface(int portNum) {
		id = portNum;
		dc = new DeterministicClient();
	}
	
	public String processMessage(String inMessage) {
		// ErrorType.kFailed.ordinal()
		assert inMessage != null;
	    System.out.printf("[%d][%s]\n", id, inMessage);
		if(inMessage.equals("exit")) 
			return null;
		String[] tokens = inMessage.split(" ");
		if(tokens[0].equals("configure")) {
			assert tokens.length == 5;
			try {
				dc.setParameters(
					Integer.parseInt(tokens[1]),
					TimeStamp.period(Long.parseLong(tokens[2])),
					Integer.parseInt(tokens[3]),
					Boolean.valueOf(tokens[4]));
			}catch(IllegalStateException ise) {
				return "ERR";
			}
			return "OK";
		}
		return inMessage;
	}
}
