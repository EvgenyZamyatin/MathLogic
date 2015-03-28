package ru.ifmo.mathlogic.zamyatin.task5.parser;

import ru.ifmo.mathlogic.zamyatin.task5.IntervalSet;
import ru.ifmo.mathlogic.zamyatin.task5.Range;

import java.util.Map;

/**
 * Created by evgeny on 28.03.15.
 */
public abstract class BinaryOperation implements Expression {
    private Expression first;
    private Expression second;

    @Override
    public String print() {
        return "(" + first.print() + ")" + operation() + "(" + second.print() + ")";
    }

    public BinaryOperation(Expression a, Expression b) {
        first = a;
        second = b;
    }

    public IntervalSet eval(Map<String, Range> mp) {
        IntervalSet a = first.eval(mp);
        IntervalSet b = second.eval(mp);
        return operation(a, b);
    }

    protected abstract IntervalSet operation(IntervalSet a, IntervalSet b);

    protected abstract String operation();


}
