package adventCalendar2016;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

class Main {
	
	static ArrayList<String> directions   = new ArrayList<Pair<Char,Int>>();
	//A list of directions

	public static void main(String[] args) throws IOException {

		loadFile("Directions");



	}
	static FileReader loadFile(String fileName) throws IOException{
		//loads any previously existing file from the files folder.

		String link = System.getProperty("java.class.path") + "//Files//" + fileName;
		File toBeLoaded = new File(link);
		FileReader reader = new FileReader(toBeLoaded);
		return reader;

	}
}
