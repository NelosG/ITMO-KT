package cargs;

import cargs.visualisation.gui.MainWindow;

import javax.swing.*;

public class GuiMain {
    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> new MainWindow().setVisible(true));
    }
}
