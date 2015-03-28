package ru.ifmo.mathlogic.zamyatin.task5.parser;

import ru.ifmo.mathlogic.zamyatin.task5.IntervalSet;
import ru.ifmo.mathlogic.zamyatin.task5.Range;

import java.util.Map;

/**
 * Created by evgeny on 28.03.15.
 */
public class Negate implements Expression {
    private Expression first;

    public Negate(Expression a) {
        first = a;
    }

    @Override
    public IntervalSet eval(Map<String, Range> map) {
        return first.eval(map).negate();
    }

    @Override
    public String print() {
        return "!" + "(" + first.print() + ")";
    }
}
