package ru.ifmo.mathlogic.zamyatin.task5;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by evgeny on 28.03.15.
 */
public class IntervalSet {
    private List<Range> mRanges;

    public IntervalSet() {
        mRanges = new ArrayList<Range>();
    }

    public IntervalSet intersect(IntervalSet a) {
        IntervalSet ans = new IntervalSet();
        for (Range r : mRanges) {
            for (Range r1 : a.mRanges) {
                ans = ans.union(r.intersect(r1));
            }
        }
        return ans;
    }

    public IntervalSet union(IntervalSet a) {
        IntervalSet ans = new IntervalSet();
        for (Range r : mRanges)
            ans = ans.union(r);
        for (Range r : a.mRanges)
            ans = ans.union(r);
        return ans;
    }

    public IntervalSet union(Range range) {
        IntervalSet ans = new IntervalSet();
        ans.mRanges.add(range);
        for (Range r : mRanges) {
            Range tmp = ans.mRanges.get(ans.mRanges.size() - 1);
            ans.mRanges.remove(ans.mRanges.size()-1);
            ans.mRanges.addAll(tmp.union(r).mRanges);
        }
        return ans;
    }

    public IntervalSet negate() {
        IntervalSet ans = new IntervalSet();
        ans = ans.union(new Range(Range.NEG_INF, Range.POS_INF));
        for (Range r : mRanges) {
            ans = ans.intersect(r.negate());
        }
        return ans;
    }

    public void openRanges() {
        for (Range r : mRanges)
            r.setClosed(false);
    }

    public List<Range> intervals() {
        return mRanges;
    }

    public String toString() {
        String ans = "{";
        for (Range r : mRanges) {
            ans += r.toString();
            ans += "; ";
        }
        ans += "}";
        return ans;
    }

    public int size() {
        return mRanges.size();
    }

    public boolean isAll() {
        return mRanges.size() == 1 && mRanges.get(0).getL() == Range.NEG_INF && mRanges.get(0).getR() == Range.POS_INF;
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof IntervalSet))
            return false;
        IntervalSet r = (IntervalSet)obj;
        return r.intervals().equals(mRanges);
    }
}
