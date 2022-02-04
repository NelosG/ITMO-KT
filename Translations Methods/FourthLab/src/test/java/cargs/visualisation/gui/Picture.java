package cargs.visualisation.gui;

import cargs.tree.Tree;
import types.Node;

import javax.imageio.ImageIO;
import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

public class Picture extends JLabel {

    BufferedImage img = null;

    public Picture() {
        super();
        setVisible(true);
    }

    public void err() {
        try {
            img = ImageIO.read(new File("./images/error.png"));
        } catch (IOException e) {
            e.printStackTrace();
        }

        setIcon(new ImageIcon(img));
    }

    public void err(Node node, double scale) {
        if (node != null) {
            img = Tree.render(node);
        }
        resizeImg(scale);
    }

    public void resizeImg(double scale) {
        setIcon(new ImageIcon(
                img.getScaledInstance(
                        (int) (img.getWidth() * scale),
                        (int) (img.getHeight() * scale),
                        Image.SCALE_SMOOTH)));
    }
}
