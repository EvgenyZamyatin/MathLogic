package ru.ifmo.mathlogic.zamyatin.task5.parser;

import ru.ifmo.mathlogic.zamyatin.task5.IntervalSet;
import ru.ifmo.mathlogic.zamyatin.task5.Range;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

/**
 * Created by evgeny on 27.03.15.
 */
public class Parser {

    private static String str;
    private static int pos;
    public static List<String> varList;

    private static Expression parseExp() {
        Expression a = parseDis();
        if (pos < str.length() && str.charAt(pos) == '-') {
            pos += 2;
            Expression b = parseExp();
            a = new BinaryOperation(a, b) {
                @Override
                protected IntervalSet operation(IntervalSet a, IntervalSet b) {
                    a = a.negate();
                    for (Range ra : a.intervals()) {
                        for (Range rb : b.intervals()) {
                            if (ra.getL() == rb.getL())
                                ra.decL();
                            if (ra.getL() == rb.getR())
                                ra.decL();
                            if (ra.getR() == rb.getL())
                                ra.incR();
                            if (ra.getR() == rb.getR())
                                ra.incR();
                        }
                    }
                    return a.union(b);
                }

                @Override
                protected String operation() {
                    return "->";
                }
            };
        }
        return a;
    }

    private static Expression parseDis() {
        Expression a = parseCon();
        if (pos < str.length() && str.charAt(pos) == '|') {
            pos += 1;
            Expression b = parseDis();
            a = new BinaryOperation(a, b) {
                @Override
                protected IntervalSet operation(IntervalSet a, IntervalSet b) {
                    return a.union(b);
                }
                @Override
                protected String operation() {
                    return "|";
                }
            };
        }
        return a;
    }

    private static Expression parseCon() {
        Expression a = parseNeg();
        if (pos < str.length() && str.charAt(pos) == '&') {
            pos += 1;
            Expression b = parseCon();
            a = new BinaryOperation(a, b) {
                @Override
                protected IntervalSet operation(IntervalSet a, IntervalSet b) {
                    return a.intersect(b);
                }
                @Override
                protected String operation() {
                    return "&";
                }
            };
        }
        return a;
    }

    private static Expression parseNeg() {
        if (pos < str.length() && Character.isLetter(str.charAt(pos))) {
            String ans = String.valueOf(str.charAt(pos++));
            while (pos < str.length() && Character.isDigit(str.charAt(pos))) {
                ans += str.charAt(pos);
                pos++;
            }
            if (!varList.contains(ans))
                varList.add(ans);
            return new Variable(ans);
        }
        if (pos < str.length() && str.charAt(pos) == '!') {
            ++pos;
            Expression a = parseNeg();
            return new Negate(a);
        }
        if (pos < str.length() && str.charAt(pos) == '(') {
            ++pos;
            Expression a = parseExp();
            ++pos;
            return a;
        }
        return null;
    }

    public static Expression parse(String s) {
        str = s;
        pos = 0;
        varList = new ArrayList<String>();
        return parseExp();
    }

    public static void main(String[] args) {
        Expression e = parse("A|!A");
        Map<String, Range> map = new TreeMap<String, Range>();
        map.put("A", new Range(0, 1));
        map.put("B", new Range(1, 2));
        System.out.println(e.eval(map));
    }
}
