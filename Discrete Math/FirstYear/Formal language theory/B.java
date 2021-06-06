import java.io.*;
import java.util.*;

public class B {
    static boolean check(String s) {
        char c;
        for (int i = 0; i < s.length(); i++) {
            c = s.charAt(i);
            if(c < 'A' || c > 'Z') return false;
        }
        return true;
    }
    public static void main(String[] args) throws IOException {
        HashMap<Character, HashSet<String>> map = new HashMap<>();
        ArrayList<Character> set = new ArrayList<>();
        Scanner sc = new Scanner(new FileInputStream("epsilon.in"));
        FileWriter fr = new FileWriter("epsilon.out");
        String s = sc.nextLine(), tmp;
        Scanner scc = new Scanner(s);
        boolean flag = true, fl;
        char tempChar;
        int n = scc.nextInt();
        for (int i = 0; i < n; i++) {
            s = sc.nextLine();
            scc = new Scanner(s);
            tempChar = scc.next().charAt(0);
            scc.next();
            if (s.length() > 5) {
                tmp = scc.next();
                if (check(tmp)) {
                    if (!map.containsKey(tempChar))
                        map.put(tempChar, new HashSet<>());
                    map.get(tempChar).add(tmp);
                }
            } else set.add(tempChar);
        }
        while (flag) {
            flag = false;
            for (Map.Entry<Character, HashSet<String>> mapEntry : map.entrySet()) {
                HashSet<String> tempSet = mapEntry.getValue();
                fl = false;
                for (String str : tempSet) {
                    for (int i = 0; i < str.length(); i++) {
                        if (!set.contains(str.charAt(i))) {
                            fl = false;
                            break;
                        } else if (i == 0) fl = true;
                    }
                    if (fl) {
                        if(!set.contains(mapEntry.getKey()))set.add(mapEntry.getKey());
                        break;
                    }
                }
                flag |= fl;
            }
        }
        set.sort(Character::compareTo);
        for (char c : set)
            fr.write(c + " ");
        fr.close();
    }
}
