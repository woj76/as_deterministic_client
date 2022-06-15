package as_deterministic_client;

public interface WorkerRunnable<ValueType> {

	public void run(ValueType element, WorkerThread t);
	
}
