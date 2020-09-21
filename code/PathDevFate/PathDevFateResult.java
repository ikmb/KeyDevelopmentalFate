import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
/**
 * @author Maryam Nazarieh
 */
public class PathDevFateResult {
	
	final private Map<String, String> PathDevFate_colors;
	final private Map<String, Gene> name_to_gene = new HashMap<String, Gene>();
	
	public PathDevFateResult(Map<String, String> PathDevFate_colors, List<Gene> all_genes) {
		this.PathDevFate_colors = PathDevFate_colors;
		
	    for (Gene gene:all_genes) {
	    	name_to_gene.put(gene.get_name(), gene);
	    }
	}
	
	public Map<String, String> getPathDevFateRoles() {
		Map<String, String> roles = new HashMap<String, String>();
	
		for (String s:PathDevFate_colors.keySet()) {
			String color = PathDevFate_colors.get(s);
			if (color.equals("black"))
        		roles.put(s, "influencer");
        	else
        		roles.put(s, "connector");
		}
		
		return roles;
	}

	public Map<String, Gene> getNameToGene() {
		return name_to_gene;
	}
}
