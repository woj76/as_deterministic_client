package as_deterministic_client;

public class TimeStamp {
	
	public enum Type { ABSOLUTE, RELATIVE; }
	
	private Long time;
	private Type type;
	
	private TimeStamp(Type type) {
		this.type = type;
		if(type == Type.ABSOLUTE) {
			time = System.nanoTime();
		}else if (type == Type.RELATIVE) {
			time = 0L;
		}
	}
	
	public static TimeStamp now() {
		return new TimeStamp(Type.ABSOLUTE);
	}
	
	public static TimeStamp period(Long period) {
		TimeStamp p = new TimeStamp(Type.RELATIVE);
		p.time = period;
		return p;
	}
	
	public TimeStamp shift(TimeStamp p) {
		assert this.type == Type.ABSOLUTE;
		assert p.type == Type.RELATIVE;
		TimeStamp r = new TimeStamp(Type.ABSOLUTE);
		r.time = this.time + p.time;
		return r;		
	}
	
	public TimeStamp elapsed(TimeStamp p) {
		assert this.type == Type.ABSOLUTE;
		assert p.type == Type.ABSOLUTE;
		return period(this.time - p.time);
	}
	
	public Long getTime() {
		return time;
	}
}
