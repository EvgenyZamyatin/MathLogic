package ru.ifmo.mathlogic.zamyatin.task5;

/**
 * Created by evgeny on 27.03.15.
 */


public class Range {
    private double mL;
    private double mR;
    public final static double NEG_INF = Double.NEGATIVE_INFINITY;
    public final static double POS_INF = Double.POSITIVE_INFINITY;

    public Range(double a, double b) {
        mL = a;
        mR = b;
    }

    public void decL() {
        mL -= 1;
    }

    public void incR() {
        mR += 1;
    }


    public IntervalSet negate() {
        IntervalSet ans = new IntervalSet();
        if (mL != NEG_INF)
            ans = ans.union(new Range(NEG_INF, mL));
        if (mR != POS_INF)
            ans = ans.union(new Range(mR, POS_INF));
        return ans;
    }

    public IntervalSet intersect(Range r) {
        double a = Math.max(mL, r.mL);
        double b = Math.min(mR, r.mR);
        IntervalSet ans = new IntervalSet();
        if (a > b)
            return ans;
        else
            return ans.union(new Range(a, b));
    }

    public IntervalSet union(Range r) {
        IntervalSet ans = new IntervalSet();
        if (mR > r.mL && mL < r.mR) {
            ans.intervals().add(new Range(Math.min(mL, r.mL), Math.max(mR, r.mR)));
            return ans;
        }
        if (mL < r.mL) {
            ans.intervals().add(this);
            ans.intervals().add(r);
            return ans;
        }
        ans.intervals().add(r);
        ans.intervals().add(this);
        return ans;
    }

    public double getL() {
        return mL;
    }

    public double getR() {
        return mR;
    }

    public String toString() {
        return "(" + mL + ", " + mR + ")";
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof Range))
            return false;
        Range r = (Range)obj;
        return r.getL() == getL() && r.getR() == getR();
    }
}