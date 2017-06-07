DM_Synth : Synth {
	// la définition du synthétiseur
	classvar def;
	// la définition des arguments
	classvar argDefs;
	// les motifs rythmiques, par signature
	classvar patsSign;
	// structure de l'interface
	classvar rowSizes;

	// les Bus employés et leurs valeurs
	var busses;
	// l'interface graphique
	var interface, knobs;

	*initClass {
		var unitF = {|v| v / 127};
		var freqF = {|v| 80 * (2 ** (v/32))};
		var lfreqF = {|v| 2 ** (v/32)};
		var timeF = {|v| 2 ** ((v/32)-4)};
		var expF = {|v| 2 ** (v/16)};
		var posnegF = {|width| {|v| width * ((v/64)-1)}};
		var dbampF = {|v| ((v/5.3333333333333)-12).dbamp};
		var intF = {|max| {|v| ((v/127)*max).round}};

		// ensemble des motifs d'une certaine longueur et subdivision
		var patterns = {|length, div|
			// motifs triés par niveau de subdivision
			var patLvls = List()!3;
			// durée du niveau 1
			var base1 = 0.5.round(div.reciprocal);
			var base2 = 1 - base1;
			// combinaisons des nombres de divisions par niveau
			var cuts = ({|i| {|j| [i,j]} ! (i+1)} ! (length+1)).flatten;
			// création des motifs
			var pats = cuts.collect {|cut|
				var l1, l2;
				// récupération des nombres de divisions
				#l1, l2 = cut;
				[
					// niveau maximum de division
					cut.select(_!=0).size,
					// séquence des fractions
					(1!(length-l1)) ++ (base1!(l1-l2)) ++ (base2!(l1-l2))
					++ ((0.5*base1)!(2*l2)) ++ ((0.5*base2)!(2*l2))
				];
			};
			// ajouter les motifs dans le niveau approprié
			pats.do {|pat| patLvls[pat[0]].add(pat[1])};
			// calculer la longueur des motifs
			patLvls = patLvls.collect(Array.newFrom(_));//.collectAs({|pat| pat}, Array));
		};

		// générer la liste des motifs suivant la longueur et la subdivision
		patsSign = [4,3].collect {|length|
			[2,3,5].collect {|div|
				patterns.(length, div)
		}};

		def = SynthDef('dream-machine',
			{
				|out = 0, amp = 0, freq = 320,
				bbr = 0, binh = 1,
				fmf = 0, fmamt = 0,
				lfreq = 4, vibamt = 0, tremamt = 0,
				acyc = 1, alog = 0,
				gamt = 0, gfreq = 4, gwidth = 0.5,
				ramt = 0, rfreq = 0,
				dist = 0,
				eqlo = 1, eqmid = 1, eqhi = 1,
				revamt = 0, revsize = 0.5, revdamp = 0.5,
				itwidth = 1, itfreq = 2,
				itlen = 0, itmode = 0, itdiv = 0,
				itbeat = 0, itpeak = 0, itatt = 0.25, itrel = 0.5,
				itkeys = 0, itoct = 0|
				// LFO
				var lfo = SinOsc.kr(2 * lfreq);
				// vibrato
				var vibrato = 2 ** (lfo * vibamt * 0.1);
				// tremolo
				var tremolo = 1 + (tremamt * lfo.abs.neg);
				// grain
				var grnfreq = gfreq * 8;
				var grnprd = gwidth/grnfreq;
				var grain =	1 + (gamt * (EnvGen.kr(Env.linen(0.1 * grnprd, 0.8 * grnprd, 0.1 * grnprd),
					Impulse.kr(grnfreq)) - 1));
				// présence cyclique
				var pcyc = [1, acyc];

				// générateur de motif
				var period = itfreq.reciprocal;
				var reset = DemandEnvGen.kr(Dseq([1,0], inf), Dseq([0.001, period*(4-itlen)-0.001], inf));
				var patsSub = (itlen * 9) + (itmode * 3) + itdiv;
				var patsCat = patsSign.flatten(2);
				// générateur de gate
				var pattern = Dswitch(
					patsCat.collect {|cat|
						Drand(cat.collect {|pat| Dshuf(pat, inf)})},
					patsSub);
				var gate = DemandEnvGen.kr(Dseq([0,2,Dseq([0,1], inf)]),
					period * Dstutter(2, pattern)
					* Dseq([itwidth, 1 - itwidth], inf), 0, reset: reset);
				// générateur d'enveloppe (itération)
				var att = 0.1 * itatt;
				var peak = 1 + itpeak;
				// temps forts
				var beats = EnvGen.kr(Env.perc(att/2, itrel, itbeat), gate-1);
				// var beats = DemandEnvGen.kr(Dpoll(Dseq([1,2,2,Dseq([1],inf)])),
				// Dseq([att/2, period-att, att/2, inf]), 1, reset: reset);
				var iter = EnvGen.kr(Env([0,peak,1,0],[att/2, att/2, itrel], releaseNode: 2), gate)
				+ beats;
				// générateur de hauteur
				var nbkeys = 2 ** itkeys;
				var nboct = 1 + itoct;
				var scale = Demand.kr(gate, reset, Dseq([
					(1 + ((Dseq([0, Drand((0..3), inf)], inf) % nbkeys) / nbkeys))
					* (2 ** (Dstutter(inf, Drand((0..3))) % nboct))
				], inf));
				var keyfreq = freq * scale;

				// FM
				var fm = 1 + (SinOsc.ar(keyfreq * (0.25 ** fmf)) * fmamt);
				// modulation en anneau
				var ring = 1 + (ramt * (SinOsc.ar(keyfreq * (2 ** rfreq)) - 1));
				// egalisateur
				var diff = ([eqlo, eqmid, eqhi] /
					([eqlo, eqmid, eqhi] * [0.3, 0.4, 0.3]).sum).differentiate;
				var equaliser = {|i|
					var oct = (i+1).log2;
					diff[0] + ((oct-1).clip(0,1) * diff[1]) + ((oct-3).clip(0,1) * diff[2]);
				};

				// le signal généré
				var sig =
				// génération de partiels
				(Mix.fill(32) {|i|
					// indice du partiel
					var num = i+1;
					// présence logarithmique
					var plog = (1 - (alog * ((i+1).log2 * 4).fold(0, 2).clip(0, 1)));
					// bruitage
					num = num + (bbr * LFNoise0.kr(500));
					// distorsion inharmonique
					num = num ** binh;
					// amplitude de base
					(0.5/((i+1)**1.2)) * (amp ** (4 ** dist))
					// égaliseur
					* equaliser.(i)
					// présence cyclique
					* pcyc[i%2]
					// présence logarithmique
					* plog
					// égaliseur
					* equaliser.(i)
					// oscillateur
					* SinOsc.ar(num
						// arpégiateur
						* keyfreq
						// modulation de fréquence
						* fm
						// vibrato
						* vibrato
					)
					}
					// tremolo
					* tremolo
					// grain
					* grain
					// modulation en anneau
					* ring
					// itérateur
					* iter
				)
				// distorsion
				** (0.25 ** dist);
				// sortie
				Out.ar(out,
					// reverb
					FreeVerb.ar(sig, revamt, revsize, revdamp)
					// stereo
					! 2);
			}
		);
		argDefs = [
			// matière
			['amp', 0, unitF],
			['freq', 64, freqF],
			['bbr', 0, unitF],
			['binh', 127, unitF],
			['fmf', 0, unitF],
			['fmamt', 0, unitF],
			['acyc', 127, unitF],
			['alog', 0, unitF],
			// entretien
			['lfreq', 64, lfreqF],
			['vibamt', 0, unitF],
			['tremamt', 0, unitF],
			['gamt', 0, unitF],
			['gfreq', 64, lfreqF],
			['gwidth', 64, unitF],
			['ramt', 0, unitF],
			['rfreq', 64, posnegF.(1)],
			// effets
			['eqlo', 64, dbampF],
			['eqmid', 64, dbampF],
			['eqhi', 64, dbampF],
			['dist', 0, unitF],
			['revamt', 0, unitF],
			['revsize', 64, unitF],
			['revdamp', 64, unitF],
			// séquenceur
			['itwidth', 127, unitF],
			['itfreq', 32, lfreqF],
			['itlen', 0, intF.(1)],
			['itmode', 0, intF.(2)],
			['itdiv', 0, intF.(2)],
			['itkeys', 0, intF.(2)],
			['itoct', 0, intF.(2)],
			// dynamique
			['itbeat', 0, unitF],
			['itpeak', 0, unitF],
			['itatt', 64, timeF],
			['itrel', 96, timeF],
		];

		rowSizes = [8,8,7,7,4]
	}

	// créer le synthétiseur
	*new {|out = 0|
		var argNames, defaults;
		var busses;
		// récupérer les arguments
		#argNames ... defaults = argDefs.flop;
		// instancier et initialiser les bus
		busses = defaults.flop.collect {|val|
			// var value = val[1].(val[0]);
			[
				Bus.control().set(val[1].(val[0])),
				val[0]
			]
		};
		// instancier le synthétiseur
		^super.new(def.name, ['out', out] ++ [argNames, busses.collect{|b| b[0].asMap}].flop.flat)
		// initialiser les champs
		.synthInit(busses);
	}

	*addDef {
		// ajouter la définition
		def.add;
	}

	synthInit {|bses|
		// enrgegistrer les Bus
		busses = bses;
	}

	setBus {|index, value|
		var val = value.round.asInteger;
		busses[index][0].set(argDefs[index][2].(val));
		busses[index][1] = val;
	}

	makeInterface {
		var row = 0, col = 0, layout;
		interface = Window("DreamMachine");
		layout = GridLayout();
		knobs = Array.newClear(argDefs.size);
		layout.addSpanning(Button().states_([["Load", Color.black, Color.gray]]).value_(0)
			.action_{Dialog.openPanel {|path| this.load(path)}}, 0, 0, columnSpan: 2);
		layout.addSpanning(Button().states_([["Save", Color.black, Color.gray]]).value_(0)
			.action_{Dialog.savePanel {|path| this.save(path)}}, 0, 2, columnSpan: 2);
		layout.addSpanning(Button().states_([["Quit", Color.black, Color.gray]]).value_(0)
			.action_{this.free}, 0, 4, columnSpan: 2);
		argDefs.do {|elt, i|
			var knob = Knob().value_(elt[1]/127).mode_(\vert)
			.action_{|v| this.setBus(i, (v.value*127).round)};
			knobs[i] = knob;
			layout.add(knob, row*2+1, col);
			layout.add(
				StaticText().string_(elt[0].asString),
				row*2+2, col);
			col = col+1;
			if (col == rowSizes[row]) {col = 0; row = row + 1};
		};
		interface.layout = layout;
		interface.front;
	}

	makeMIDIInterface {|layout =
		(Platform.userExtensionDir +/+ "DreamMachine" +/+ "extras" +/+ "MIDIMaps" +/+ "BCR.midimap.scd")|
		var map = if (File.exists(layout))
		{
			var file = File(layout, "r");
			var res = file.readAllString.interpret;
			file.close;
			res;
		} {
			// fallback
			"MIDI map load failed !".postln;
		};

		if (map.notNil) {
			var out;
			MIDIClient.init(1, 1);
			// initialisation du controlleur externe
			out = MIDIOut(0);
			busses.do {|bus, i| out.control(0, map[i], bus[1])};
			// initialisation du récepteur MIDI
			MIDIFunc.cc({|val, num, chan, src|
				this.setBus(map.indexOf(num), val);
				// DEBUG
				// "%: %".format(chan, num).postln;
			}, map);
		};
	}

	free {
		// fermer l'interface si elle existe
		if (interface.notNil) {interface.close; interface = nil};
		// libérer le Synth
		super.free;
		// libérer les Bus
		busses.do(_.free);
	}

	save {|path|
		var file;
		file = File(path, "w");
		file.write([argDefs.flop[0], busses.collect(_[1])].flop.asCompileString);
		file.close;
	}

	load {|path|
		var file;
		if (File.exists(path)) {
			var content, names;
			file = File(path, "r");
			content = file.readAllString;
			file.close;
			content = content.interpret;
			names = argDefs.flop[0];
			content.do {|elt|
				var index = names.indexOf(elt[0]);
				var value = elt[1];
				if (index.notNil) {
					this.setBus(index, value);
					if (interface.notNil) {knobs[index].value = value/127};
				};
			};
		};
	}

	// for reverse compatibility reasons:
	oldload {|path|
		var file;
		if (File.exists(path)) {
			var content, names;
			"FOUND".postln;
			file = File(path, "r");
			content = file.readAllString;
			file.close;
			content = content.interpret;
			names = argDefs.flop[0];
			content.do {|elt|
				var index = names.indexOf(elt[0]);
				var value = elt[1];
				if (index.notNil) {
					this.setBus(index, value * 127);
					if (interface.notNil) {knobs[index].value = value};
				};
			};
		};
	}
}