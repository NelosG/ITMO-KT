package cargs.visualisation.gui;

import cargs.CArgsLexer;
import cargs.CArgsParser;

import javax.swing.*;
import java.awt.*;

public class MainWindow extends JFrame {
    private JPanel root;
    private JButton runButton;
    private JTextArea textArea;
    private JScrollPane scrollPane;
    private JLabel picture;
    private JButton minButton;
    private JButton maxButton;

    private double scale = 1.;

    public MainWindow() {
        super("Parser");
        setContentPane(root);
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        int sizeWidth = (int) (screenSize.width * 0.95);
        int sizeHeight = (int) (screenSize.height * 0.95);
        setBounds((screenSize.width - sizeWidth) / 2 - 1, 0, sizeWidth, sizeHeight);
        setDefaultCloseOperation(EXIT_ON_CLOSE);

        runButton.addActionListener(e -> {
            try {
                ((Picture) picture).err(new CArgsParser(new CArgsLexer(textArea.getText())).parse(), scale);
            } catch (Exception ex) {
                ((Picture) picture).err();
            }
        });
        minButton.addActionListener(e -> {
            scale *= 0.85;
            ((Picture) picture).resizeImg(scale);
        });
        maxButton.addActionListener(e -> {
            scale *= 1.15;
            ((Picture) picture).resizeImg(scale);
        });
        scrollPane.getVerticalScrollBar().setUnitIncrement(16);
        scrollPane.getHorizontalScrollBar().setUnitIncrement(16);
    }

    private void createUIComponents() {
        picture = new Picture();
    }
}
