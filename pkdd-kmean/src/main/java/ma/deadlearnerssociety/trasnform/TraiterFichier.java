package ma.deadlearnerssociety.trasnform;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

public class TraiterFichier {

    public static String exractAllPolylines(String polyline){
        String newPolyline = polyline.replace("[[", "")
                .replace("]]", "")
                .replace("],[", "\n");
        return newPolyline;
    }

    public static void traiterFichier() throws IOException{
        File fin = new File ("/Users/macpro/PycharmProjects/pkdd-15-predict-taxi-service-trajectory-i/data/train.csv");
        InputStream ips= new FileInputStream(fin);
        BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(ips));

        File fout = new File (("/Users/macpro/PycharmProjects/pkdd-15-predict-taxi-service-trajectory-i/data/allPolylines.csv"));
        FileWriter fw = new FileWriter (fout);
        String line = null;

        fw.write("LONG,LAT\n");

        boolean firstLine = true;

        System.out.println("Start Processing...");

        while((line=bufferedReader.readLine())!=null){
            if(!firstLine) {
                String[] ll = line.split("\",\"");
                String polyline = ll[8]; // Polyline dans la 9ième position dans le fichier TRAIN
                String polylines = String.valueOf(exractAllPolylines(polyline)).replace("\"", "\n");
                fw.write (polylines);
            }
            firstLine = false;
        }

        bufferedReader.close();
        fw.close();

        System.out.println("Done!");
    }

    public static void main(String[] args) throws IOException {
        //String polyline = "[[-8.585676,41.148522],[-8.585712,41.148639],[-8.585685,41.148855],[-8.58573,41.148927],[-8.585982,41.148963],[-8.586396,41.148954],[-8.586072,41.14872],[-8.586324,41.147847],[-8.586999,41.14746],[-8.586576,41.147154],[-8.584884,41.146623]]";

        //System.out.println(exractAllPolylines(polyline));

        traiterFichier();

    }

}