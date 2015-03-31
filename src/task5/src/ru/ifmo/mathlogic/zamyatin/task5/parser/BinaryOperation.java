package ru.ifmo.mathlogic.zamyatin.task5.parser;

import ru.ifmo.mathlogic.zamyatin.task5.IntervalSet;
import ru.ifmo.mathlogic.zamyatin.task5.Range;

import java.util.Map;

/**
 * Created by evgeny on 28.03.15.
 */
public abstract class BinaryOperation implements Expression {
    protected Expression first;
    protected Expression second;
    protected String name;


    @Override
    public String getName() {
        return name;
    }

    @Override
    public void setName(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return "(" + first.toString() + ")" + operation() + "(" + second.toString() + ")";
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
