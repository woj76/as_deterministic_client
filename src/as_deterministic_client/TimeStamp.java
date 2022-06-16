package as_deterministic_client;

import java.time.Instant;
import java.time.ZoneId;


/*
 * The Instant class in the standard Java API has all this functionality and
 * could be used instead. In fact, it is used a bit nevertheless to get the
 * pretty printed absolute time stamp.
 */
public class TimeStamp {
	
	public enum Type { ABSOLUTE, RELATIVE; }
	
	private Long time;
	private Instant refPoint = null; 
	private Type type;
	
	private TimeStamp(Type type) {
		this.type = type;
		if(type == Type.ABSOLUTE) {
			time = System.nanoTime();
			refPoint = Instant.now().minusNanos(time);
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
		r.refPoint = this.refPoint; // probably obsolete...
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
	
	public String toString() {
		if(type == Type.RELATIVE) {
			return "Interval["+time+"ns.]";
		}
		return "Absolute[" +time+","+refPoint.plusNanos(time).atZone(ZoneId.systemDefault()).toString()+"]";
	}
}
