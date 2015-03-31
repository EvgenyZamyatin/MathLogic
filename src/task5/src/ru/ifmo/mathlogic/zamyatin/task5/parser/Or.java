package ru.ifmo.mathlogic.zamyatin.task5.parser;

import ru.ifmo.mathlogic.zamyatin.task5.IntervalSet;

/**
 * Created by evgeny on 30.03.15.
 */
public class Or extends BinaryOperation {
    public Or(Expression a, Expression b) {
        super(a, b);
    }

    @Override
    protected IntervalSet operation(IntervalSet a, IntervalSet b) {
        return a.union(b);
    }
    @Override
    protected String operation() {
        return "|";
    }
}
