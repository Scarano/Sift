package structureextractor.util;

public class LogSum {
    // If x / y > exp(LOGSUM_THRESHOLD), then logSum(lx, ly) short-circuits to lx.
		// For Doubles, set to 745.0 to avoid any loss of precision.
    // By setting lower, we avoid slow calls to log1p and exp that may have little effect on
    // results.
    static final double LOGSUM_THRESHOLD = 9.0;

    public static double logSum(double lx, double ly) {
        if (lx == Double.NEGATIVE_INFINITY) return ly;
        if (ly == Double.NEGATIVE_INFINITY) return lx;
        double d = lx - ly;
        if (d >= 0) {
            if (d > LOGSUM_THRESHOLD) return lx;
            else return lx + Math.log1p(Math.exp(-d));
        }
        else {
            if (d < -LOGSUM_THRESHOLD) return ly;
            else return ly + Math.log1p(Math.exp(d));
        }
    }
}
