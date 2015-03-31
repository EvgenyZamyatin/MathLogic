package ru.ifmo.mathlogic.zamyatin.task5.parser;

import ru.ifmo.mathlogic.zamyatin.task5.IntervalSet;

/**
 * Created by evgeny on 30.03.15.
 */
public class Impl extends BinaryOperation {

    public Impl(Expression a, Expression b) {
        super(a, b);
    }

    @Override
    protected IntervalSet operation(IntervalSet a, IntervalSet b) {
        a = a.negate();
        a = a.union(b);
        a.openRanges();
        return a;
    }

    @Override
    protected String operation() {
        return "->";
    }

}
