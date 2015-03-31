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
            a = new Impl(a, b);
        }
        return a;
    }

    private static Expression parseDis() {
        Expression a = parseCon();
        if (pos < str.length() && str.charAt(pos) == '|') {
            pos += 1;
            Expression b = parseDis();
            a = new Or(a, b);
        }
        return a;
    }

    private static int counter=1;

    private static class K {
        public Expression kMinus;
        public Expression kPlus;
        public K(Expression a, Expression b) {
            kMinus = a;
            kPlus = b;
        }
    }

    public static Expression toSimpleImplication(Expression e) {
        K k = calcK(e);
        Expression ans = k.kPlus;
        ans = new And(ans, new Negate(new Variable("P0")));
        return ans = new Impl(ans, new Variable("P"+counter++));
    }

    private static K calcK(Expression e) {
        e.setName("P"+counter++);
        if (e instanceof Variable) {
            Expression a = parse(((Variable) e).name + "->" + ((Variable) e).name);
            Expression b = parse(((Variable) e).name + "->" + ((Variable) e).name);
            return new K(a, b);
        }
        if (e instanceof BinaryOperation) {
            K ka = calcK(((BinaryOperation) e).first);
            K kb = calcK(((BinaryOperation) e).second);
            String pa = ((BinaryOperation) e).first.getName();
            String pb = ((BinaryOperation) e).second.getName();
            if (e instanceof And) {

                Expression pl = new And(ka.kPlus, new And(kb.kPlus, new Impl(new And(new Variable(pa), new Variable(pb)), new Variable(e.getName()))));
                Expression mn = new And(ka.kMinus, new And(kb.kMinus, new And(new Impl(new Variable(e.getName()), new Variable(pa)), new Impl(new Variable(e.getName()), new Variable(pb)))));
                return new K(mn, pl);
            }
            if (e instanceof Or) {
                Expression pl = new And(ka.kPlus, new And(kb.kPlus, new And(new Impl(new Variable(pa), new Variable(e.getName())), new Impl(new Variable(pb), new Variable(e.getName())))));
                Expression mn = new And(ka.kMinus, new And(kb.kMinus, new Impl(new Variable(e.getName()), new Or(new Variable(pa), new Variable(pb)))));
                return new K(mn, pl);
            }
            if (e instanceof Impl) {
                Expression pl = new And(ka.kMinus, new And(kb.kPlus, new Impl(new Impl(new Variable(pa), new Variable(pb)), new Variable(e.getName()))));
                Expression mn = new And(ka.kPlus, new And(kb.kMinus, new Impl(new And(new Variable(e.getName()), new Variable(pa)), new Variable(pb))));
                return new K(mn, pl);
            }
            throw new NullPointerException();
        }
        if (e instanceof Negate) {
            K k = calcK(((Negate) e).first);
            String pa = e.getName();
            Expression pl = new And(k.kMinus, new Impl(new Impl(new Variable(pa), new Variable("P0")), new Variable(e.getName())));
            Expression mn = new And(k.kPlus, new Impl(new And(new Variable(e.getName()), new Variable(pa)), new Variable("P0")));
            return new K(mn, pl);
        }
        throw new NullPointerException();
    }

    private static Expression parseCon() {
        Expression a = parseNeg();
        if (pos < str.length() && str.charAt(pos) == '&') {
            pos += 1;
            Expression b = parseCon();
            a = new And(a, b);
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
