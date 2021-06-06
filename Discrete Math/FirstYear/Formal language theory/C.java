import java.io.*;
import java.util.*;

public class C {
    static HashMap<Character, HashSet<String>> map = new HashMap<>();
    static HashSet<Character> dost = new HashSet<>();
    static HashSet<Character> por = new HashSet<>();
    static HashSet<Character> all = new HashSet<>();

    static boolean term(String str) {
        boolean flag = true;
        for (int i = 0; i < str.length(); i++) {
            if (str.charAt(i) < 'a' || str.charAt(i) > 'z') {
                flag = false;
                all.add(str.charAt(i));
            }
        }
        return flag;
    }

    static boolean por(String str) {
        for (int i = 0; i < str.length(); i++) {
            if (str.charAt(i) < 'a' || str.charAt(i) > 'z')
                if(!por.contains(str.charAt(i)))
                    return false;
        }
        return true;
    }

    public static void main(String[] args) throws IOException {
        Scanner sc = new Scanner(new FileInputStream("useless.in"));
        FileWriter fr = new FileWriter("useless.out");
//        Scanner sc = new Scanner(System.in);
        ArrayList<Character> res = new ArrayList<>();
        String s = sc.nextLine(), tmp;
        Scanner scc = new Scanner(s);
        boolean flag = true, fl;
        char tempChar;
        int n = scc.nextInt();
        char start = scc.next().charAt(0);
        dost.add(start);
        all.add(start);
        for (int i = 0; i < n; i++) {
            s = sc.nextLine();
            scc = new Scanner(s);
            tempChar = scc.next().charAt(0);
            all.add(tempChar);
            scc.next();
            if (s.length() > 5) {
                tmp = scc.next();
                if (!map.containsKey(tempChar))
                    map.put(tempChar, new HashSet<>());
                map.get(tempChar).add(tmp);
                if(term(tmp)) por.add(tempChar);
            } else por.add(tempChar);
        }
        while (flag) {
            flag = false;
            for (Map.Entry<Character, HashSet<String>> mapEntry : map.entrySet()) {
                HashSet<String> tempSet = mapEntry.getValue();
                fl = false;
                if(!por.contains(mapEntry.getKey())) {
                    for (String str : tempSet) {
                        for (int i = 0; i < str.length(); i++) {
                            if (i == 0) fl = true;
                            if (str.charAt(i) < 'a' || str.charAt(i) > 'z') {
                                if (!por.contains(str.charAt(i))) {
                                    fl = false;
                                    break;
                                }
                            }
                        }
                        if (fl) {
                            por.add(mapEntry.getKey());
                            break;
                        }
                    }
                }
                flag |= fl;
            }
        }
        flag = true;
        while (flag) {
            flag = false;
            for (Map.Entry<Character, HashSet<String>> mapEntry : map.entrySet()) {
                if(dost.contains(mapEntry.getKey())) {
                    HashSet<String> tempSet = mapEntry.getValue();
                    for (String str : tempSet) {
                        if(por(str)) {
                            for (int i = 0; i < str.length(); i++) {
                                if (str.charAt(i) < 'a' || str.charAt(i) > 'z') {
                                    if(!dost.contains(str.charAt(i))) {
                                        dost.add(str.charAt(i));
                                        flag = true;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        for(char c : all) {
            if(!por.contains(c) || !dost.contains(c)) {
                if(!res.contains(c)) res.add(c);
            }
        }
        res.sort(Character::compareTo);
        for (char c : res) {
//            System.out.print(c + " ");
            fr.write(c + " ");
        }
        fr.close();
    }
}
