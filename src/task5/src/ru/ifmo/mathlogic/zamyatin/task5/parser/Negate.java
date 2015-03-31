package ru.ifmo.mathlogic.zamyatin.task5.parser;

import ru.ifmo.mathlogic.zamyatin.task5.IntervalSet;
import ru.ifmo.mathlogic.zamyatin.task5.Range;

import java.util.Map;

/**
 * Created by evgeny on 28.03.15.
 */
public class Negate implements Expression {
    protected Expression first;
    protected String name;
    public Negate(Expression a) {
        first = a;
    }

    @Override
    public IntervalSet eval(Map<String, Range> map) {
        IntervalSet ans = first.eval(map).negate();
        ans.openRanges();
        return ans;
    }

    @Override
    public String toString() {
        return "!" + "(" + first.toString() + ")";
    }

    @Override
    public void setName(String name) {
        this.name = name;
    }

    @Override
    public String getName() {
        return name;
    }
}
