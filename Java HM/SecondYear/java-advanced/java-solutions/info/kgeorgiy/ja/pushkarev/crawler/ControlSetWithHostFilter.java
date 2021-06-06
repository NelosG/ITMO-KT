package info.kgeorgiy.ja.pushkarev.crawler;

import info.kgeorgiy.java.advanced.crawler.URLUtils;

import java.net.MalformedURLException;
import java.util.List;
import java.util.Set;

public class ControlSetWithHostFilter extends ControlSet {

    private final List<String> hosts;

    ControlSetWithHostFilter(final Set<String> set, List<String> hosts) {
        super(set);
        this.hosts = hosts;
    }

    @Override
    public boolean check(String s) {
        if(super.check(s)) {
            try {
                return hosts.contains(URLUtils.getHost(s));
            } catch (MalformedURLException e) {
                return false; //may be return true to put in failed urls
            }
        }
        return false;
    }
}
