package dotsandboxes;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.MouseInputAdapter;
import java.util.ArrayList;

public class DotsAndBoxesUI {

    static final int lineLength = 32;
    static final int margin = 10;
    static final int gap = 0;
    static final int dotDiameter = 6;

    // The coordinate of the top or left of a the painting area for this row/col
    private static int corner(int col) {
        return margin + col * (gap + lineLength + gap + dotDiameter);
    }

    /** Colours for the different players. Only goes up to 5. */
    static final Color[] playerColours = { Color.WHITE, Color.RED, Color.BLUE, Color.GREEN, Color.PINK, Color.ORANGE };

    final DotsAndBoxesGrid grid;
    final JPanel anchorPane;
    final Canvas canvas;
    final JLabel label;

    private void updateLabel() {
        label.setForeground(playerColours[grid.getPlayer()]);
        label.setText(String.format("Player %d's turn", grid.getPlayer()));
    }

    public DotsAndBoxesUI(final DotsAndBoxesGrid grid) {
        this.grid = grid;
        anchorPane = new JPanel(new BorderLayout());

        label = new JLabel("");
        updateLabel();

        canvas = new DABCanvas();

        anchorPane.add(canvas, BorderLayout.CENTER);
        anchorPane.add(label, BorderLayout.NORTH);

        grid.addConsumer((g) -> {
            updateLabel();
            canvas.repaint();
        });
    }

    /** A component that paints and handles clicks on the game */
    class DABCanvas extends Canvas {

        final ArrayList<Horizontal> horizontals = new ArrayList<>();
        final ArrayList<Vertical> verticals = new ArrayList<>();
        final ArrayList<Box> boxes = new ArrayList<>();

        /** Represents a horizontal line. */
        record Horizontal(int col, int row) {
            Rectangle rect() {
                int x = corner(col) + dotDiameter + gap;
                int y = corner(row);
                return new Rectangle(x, y, lineLength, dotDiameter);
            }

            /** Whether or not this line contains this point */
            boolean contains(int x, int y) {
                return rect().contains(x, y);
            }

            /** Paints this element, based on the passed in grid */
            public void draw(DotsAndBoxesGrid grid, Graphics2D g2d) {
                g2d.setColor(grid.getHorizontal(col, row) ? Color.DARK_GRAY : Color.LIGHT_GRAY);
                g2d.fill(this.rect());
            }
        }

        /** Represents a horizontal line. */
        record Vertical(int col, int row) {
            Rectangle rect() {
                int x = corner(col);
                int y = corner(row) + dotDiameter + gap;
                return new Rectangle(x, y, dotDiameter, lineLength);
            }

            /** Whether or not this line contains this point */
            boolean contains(int x, int y) {
                return rect().contains(x, y);
            }

            /** Paints this element, based on the passed in grid */
            public void draw(DotsAndBoxesGrid grid, Graphics2D g2d) {
                g2d.setColor(grid.getVertical(col, row) ? Color.DARK_GRAY : Color.LIGHT_GRAY);
                g2d.fill(this.rect());
            }
        }

        /** represents a box */
        record Box(int col, int row) {
            Rectangle rect() {
                int x = corner(col) + dotDiameter + gap;
                int y = corner(row) + dotDiameter + gap;
                return new Rectangle(x, y, lineLength, lineLength);
            }

            /** Whether or not this line contains this point */
            boolean contains(int x, int y) {
                return rect().contains(x, y);
            }

            /** Paints this element, based on the passed in grid */
            public void draw(DotsAndBoxesGrid grid, Graphics2D g2d) {
                g2d.setColor(playerColours[grid.getBoxOwner(col, row)]);
                g2d.fill(this.rect());
            }
        }


        public DABCanvas() {
            // Size the canvas to just contain the elements
            int width = corner(grid.width) + margin;
            int height = corner(grid.height) + margin;
            this.setPreferredSize(new Dimension(width, height));

            // Create records for the boxes
            for (int row = 0; row < grid.height - 1; row++) {
                for (int col = 0; col < grid.width - 1; col++) {
                    boxes.add(new Box(col, row));
                }
            }

            // Create records for horizontals
            for (int row = 0; row < grid.height; row++) {
                for (int col = 0; col < grid.width - 1; col++) {
                    horizontals.add(new Horizontal(col, row));
                }
            }

            // Create records for verticals
            for (int row = 0; row < grid.height - 1; row++) {
                for (int col = 0; col < grid.width; col++) {
                    verticals.add(new Vertical(col, row));
                }
            }

            addMouseListener(new MouseInputAdapter() {
                @Override public void mousePressed(MouseEvent e) {
                    for (Horizontal h : horizontals) {
                        if (h.contains(e.getX(), e.getY())) {
                            grid.drawHorizontal(h.col(), h.row(), grid.getPlayer());
                        }
                    }

                    for (Vertical v : verticals) {
                        if (v.contains(e.getX(), e.getY())) {
                            grid.drawVertical(v.col(), v.row(), grid.getPlayer());
                        }
                    }
                }        
            });

        }

        @Override public void paint(Graphics g) {
            Graphics2D g2d = (Graphics2D)g;
            g.clearRect(0, 0, this.getWidth(), this.getHeight());
            g2d.setColor(Color.WHITE);
            g.fillRect(0, 0, this.getWidth(), this.getHeight());

            // Paint the boxes
            for (Box b : boxes) {
                b.draw(grid, g2d);
            }

            // Paint the horizontals
            for (Horizontal h : horizontals) {
                h.draw(grid, g2d);
            }

            // Paint the boxes
            for (Vertical v : verticals) {
                v.draw(grid, g2d);
            }

            // Draw the dots
            for (int row = 0; row < grid.height; row++) {
                for (int col = 0; col < grid.width; col++) {
                    g2d.setColor(Color.BLACK);
                    g2d.fillOval(corner(col), corner(row), dotDiameter, dotDiameter);
                }
            }
        }

    }




}
