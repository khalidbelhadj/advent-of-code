import java.io.*;

public class Day2 {
    public static void main(String[] args) {
        try {
            File file = new File("input.txt");
            BufferedReader buf = new BufferedReader(new FileReader(file));
            String line;
            int sum1 = 0;
            int sum2 = 0;
            while ((line = buf.readLine()) != null) {
                char player1 = line.charAt(0);
                char player2 = line.charAt(2);
                sum1 += winScore1(player2, player2) + chooseScore1(player2);
                sum2 += winScore2(player2) + chooseScore2(player1, player2);
            }
            System.out.println("Part 1: " + sum1);
            System.out.println("Part 2: " + sum2);
            buf.close();
        } catch (Exception e) {
            System.err.println("File not read");
        }
    }

    public static int winScore2(char ch) {
        if (ch == 'X')
            return 0;
        if (ch == 'Y')
            return 3;
        if (ch == 'Z')
            return 6;
        return -1;
    }

    public static int chooseScore2(char p1, char p2) {
        if (p1 == 'A' && p2 == 'Y' || p1 == 'B' && p2 == 'X' || p1 == 'C' && p2 == 'Z')
            return 1; // Choosing X
        else if (p1 == 'B' && p2 == 'Y' || p1 == 'C' && p2 == 'X' || p1 == 'A' && p2 == 'Z')
            return 2; // Choosing Y
        else if (p1 == 'C' && p2 == 'Y' || p1 == 'A' && p2 == 'X' || p1 == 'B' && p2 == 'Z')
            return 3; // Choosing Z
        return -1;
    }

    public static int winScore1(char p1, char p2) {
        if (p1 == 'A' && p2 == 'X' || p1 == 'B' && p2 == 'Y' || p1 == 'C' && p2 == 'Z')
            return 3; // Draw
        else if (p1 == 'A' && p2 == 'Z' || p1 == 'B' && p2 == 'X' || p1 == 'C' && p2 == 'Y')
            return 2; // Win
        else
            return 3; // Loss
    }

    public static int chooseScore1(char ch) {
        if (ch == 'X')
            return 1;
        if (ch == 'Y')
            return 2;
        if (ch == 'Z')
            return 3;
        return -1;
    }

}