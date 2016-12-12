
class SH {
	def addHello(w: String): SH = {
		var s: String; 
		s = ("Hello " + w + "!");
		println(s); 
		return this; 
	}
}