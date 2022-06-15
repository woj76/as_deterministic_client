package as_deterministic_client;

public enum ActivationReturnType {

	kRegisterServices(0), kServiceDiscovery(1),	kInit(2), kRun(3), kTerminate(4); 

	private final int code;
	
	private ActivationReturnType(int code) {
		this.code = code;
	}
	
	public int code() { return code; }
}
