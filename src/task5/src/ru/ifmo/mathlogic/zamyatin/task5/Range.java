package ru.ifmo.mathlogic.zamyatin.task5;

/**
 * Created by evgeny on 27.03.15.
 */


public class Range {
    private double mL;
    private double mR;
    private boolean mLClosed;
    private boolean mRClosed;

    public final static double NEG_INF = Double.NEGATIVE_INFINITY;
    public final static double POS_INF = Double.POSITIVE_INFINITY;

    public Range(double a, double b) {
        mL = a;
        mR = b;
        mLClosed = false;
        mRClosed = false;
    }

    public Range(double a, double b, boolean lClosed, boolean rClosed) {
        mL = a;
        mR = b;
        this.mLClosed = lClosed;
        this.mRClosed = rClosed;
    }

    public IntervalSet negate() {
        IntervalSet ans = new IntervalSet();
        if (mL != NEG_INF)
            ans = ans.union(new Range(NEG_INF, mL, false, !mLClosed));
        if (mR != POS_INF)
            ans = ans.union(new Range(mR, POS_INF, !mRClosed, false));
        return ans;
    }

    public IntervalSet intersect(Range r) {
        boolean lc, rc;
        double a;
        double b;
        if (mL < r.mL) {
            a = r.mL;
            lc = r.mLClosed;
        } else {
            a = mL;
            lc = mLClosed;
        }
        if (mL == r.mL)
            lc = mLClosed && r.mLClosed;

        if (mR < r.mR) {
            b = mR;
            rc = mRClosed;
        } else {
            b = r.mR;
            rc = r.mRClosed;
        }

        if (mR == r.mR)
            rc = mRClosed && r.mRClosed;
        Range nr = new Range(a, b, lc, rc);
        IntervalSet ans = new IntervalSet();
        if (nr.isEmpty())
            return ans;
        else
            return ans.union(nr);
    }

    public boolean isEmpty() {
        return !(mR - mL > 0 || mR == mL && mLClosed && mRClosed);
    }

    private Range unite(Range r) {
        boolean lc, rc;
        if (mL < r.mL)
            lc = mLClosed;
        else
            lc = r.mLClosed;
        if (mL == r.mL)
            lc = mLClosed || r.mLClosed;
        if (mR < r.mR)
            rc = r.mRClosed;
        else
            rc = mRClosed;
        if (mR == r.mR)
            rc = mRClosed || r.mRClosed;
        return new Range(Math.min(mL, r.mL), Math.max(mR, r.mR), lc, rc);
    }

    public IntervalSet union(Range r) {
        IntervalSet ans = new IntervalSet();
        if ((mR > r.mL && mL < r.mR)) {
            ans.intervals().add(unite(r));
            return ans;
        }

        if ((mR == r.mL && (mRClosed || r.mLClosed))) {
            ans.intervals().add(unite(r));
            return ans;
        }

        if ((mL == r.mR && (mLClosed || r.mRClosed))) {
            ans.intervals().add(unite(r));
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

    public void setClosed(boolean closed) {
        mLClosed = mRClosed = closed;
    }

    public String toString() {
        return (mLClosed ? "[" : "(") + mL + ", " + mR + (mRClosed ? "]" : ")");
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof Range))
            return false;
        Range r = (Range)obj;
        return r.getL() == getL() && r.getR() == getR();
    }
}