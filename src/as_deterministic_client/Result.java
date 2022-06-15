package as_deterministic_client;

public class Result<T> {
	private T return_value = null;
	private ErrorType error_value = null;
	
	public Result() {}
	
	public Result(T v) {
		return_value = v;
	}
	
	public Result(ErrorType e) {
		error_value = e;
	}
	
	public boolean hasValue() {
		return return_value != null;
	}
	
	public T getValue() {
		return return_value;
	}
	
	public ErrorType getError() {
		return error_value;
	}
}
