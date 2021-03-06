package ru.ifmo.mathlogic.zamyatin.task5.parser;


import ru.ifmo.mathlogic.zamyatin.task5.IntervalSet;
import ru.ifmo.mathlogic.zamyatin.task5.Range;

import java.util.Map;

/**
 * Created by evgeny on 28.03.15.
 */
public class Variable implements Expression {
    protected String name;

    public Variable(String name) {
        this.name = name;
    }

    @Override
    public IntervalSet eval(Map<String, Range> map) {
        return new IntervalSet().union(map.get(name));
    }

    @Override
    public String toString() {
        return name;
    }

    @Override
    public void setName(String name) {
        return;
    }

    @Override
    public String getName() {
        return name;
    }
}
