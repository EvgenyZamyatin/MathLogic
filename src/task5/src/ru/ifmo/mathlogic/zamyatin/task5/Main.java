package ru.ifmo.mathlogic.zamyatin.task5;

import ru.ifmo.mathlogic.zamyatin.task5.parser.Expression;
import ru.ifmo.mathlogic.zamyatin.task5.parser.Parser;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.*;

/**
 * Created by evgeny on 28.03.15.
 */
public class Main {

    private static boolean found;
    private static Map<String, Range> ans;
    private static Expression e;
    static int cnt = 0;
    private static List<String> varList;

    private static double getValue(int i, int n) {
        double l = -1;
        if (i == 0)
            return Range.NEG_INF;
        if (i == n - 1)
            return Range.POS_INF;
        return i;
    }

    private static void gen(int pos, Map<String, Range> map, int n) {
        if (pos == varList.size()) {
            ++cnt;
            IntervalSet t = e.eval(map);
            if (!t.isAll()) {
                found = true;
                ans = map;
            }
            return;
        }
        String name = varList.get(pos);
        for (int i = 0; i < n - 1 && !found; i++) {
            for (int j = Math.max(1, i); j < n && !found; j++) {
                map.put(name, new Range(getValue(i, n), getValue(j, n)));
                gen(pos+1, map, n);
            }
        }
    }

    public static void main(String[] args) throws FileNotFoundException {
        Scanner sc = new Scanner(new File("task5.in"));
        PrintWriter out = new PrintWriter(new File("task5.out"));
        e = Parser.parse(sc.next());
        sc.close();
        found = false;
        varList = Parser.varList;
        gen(0, new TreeMap<String, Range>(), Parser.varList.size() * 3 + 2);
        if (found) {
            for (String key : ans.keySet()) {
                out.println(key + " " + ans.get(key));
            }
        } else {
            out.println("Формула общезначима");
        }
        System.out.println(cnt);
        out.close();
    }
}
