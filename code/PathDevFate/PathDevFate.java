import java.io.*;
import java.util.Map;
import java.util.HashMap;
import java.util.*;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;
import java.lang.*;
/**
 * PathDevFate core class
 * @author Maryam Nazarieh
 */
public class PathDevFate{
	public static ArrayList<String> ReadInputFile(String file_input){
		ArrayList<String> arr = new ArrayList<String>();
		try{
			BufferedReader br = null;
			br = new BufferedReader(new FileReader(file_input));
			
			String strRead = null;
			while((strRead = br.readLine())!= null){
				arr.add(strRead);
			}
			br.close();
		}catch(IOException e){
			System.out.println("Error reading file " + file_input + ".");
			System.exit(1);
		}
		return arr;
	}
	
	public static void writeOutput(PathDevFateResult PathDevFate_result, String path) {
		List<String> output = new LinkedList<String>();
		
		// build output data
		output.add("Gene,PathDevFate_role,out_degree,in_degree");
		Map<String, String> PathDevFate_roles = PathDevFate_result.getPathDevFateRoles();
		
		for (String gene:PathDevFate_roles.keySet())
			output.add(gene + "," + PathDevFate_roles.get(gene) + "," + PathDevFate_result.getNameToGene().get(gene).get_degree_succ() + "," + PathDevFate_result.getNameToGene().get(gene).get_degree_pred());
		
		try {
			
			BufferedWriter bw = null;
			bw = new BufferedWriter(new FileWriter(path));
			
			for (String entry:output) {
				bw.write(entry);
				bw.newLine();
			}
			bw.close();
		} catch (IOException e) {
			System.err.println("Error writing file" + path + ".");
		}
	}
	public static PathDevFateResult KDCF(String input, ArrayList<String> arr){
			ArrayList<Gene> KDCF = null;
			ArrayList<Gene> nw = null;
			Component c = new Component();
			ArrayList<Gene> largest_ConnectedComponent = null;
			ArrayList<String> geneList = Network.findDistinctGene(arr);

			if(input.equals("LCCD")){
				nw = Network.construct_network_CC("LCCD",geneList,arr);
				largest_ConnectedComponent = c.FindConnectedComponents(nw,geneList,arr,"LCCD");
			}
			else if(input.equals("LCC") || input.equals("SCC")){
				nw = Network.construct_network_CC("LCC",geneList,arr);
				if(input.equals("LCC")){
					largest_ConnectedComponent = c.FindConnectedComponents(nw,geneList,arr,"LCC");
				}
				else{
					largest_ConnectedComponent = c.Find_SCC(nw,geneList,arr);
				}
			}
				ArrayList<Gene> ds = ComputePathDevFate.dominitee_set(largest_ConnectedComponent);
				ArrayList<Gene> black_darkGray = ComputePathDevFate.findConnectors(largest_ConnectedComponent,ds);
				
				KDCF = ComputePathDevFate.mimicDS(largest_ConnectedComponent,black_darkGray);
				System.out.println(KDCF);
				
				Map<String, String> PathDevFate_colors = new HashMap<String, String>();
				for (Gene gene:KDCF) 
					PathDevFate_colors.put(gene.get_name(), gene.get_color());
				
				return new PathDevFateResult(PathDevFate_colors, nw);
				
	}
	
	//****************************************
	public static void main(String[] args) {
	
		String[] parameters = args;
		// read input-file
		ArrayList<String> arr = ReadInputFile(parameters[1]);
		// compute PathDevFate
		PathDevFateResult KDCF_result = KDCF(parameters[0], arr);
		// write to output-file
		writeOutput(KDCF_result, parameters[2]);
	}

}
