(
{
	var machines;
	Server.default.bootSync;
	DM_Synth.addDef;
	Server.default.sync;
	machines = {DM_Synth()} ! 4;
	DM_Synth.makeMIDIInterface;
}.fork
)

// (
// {
// 	var machine;
// 	Server.default.bootSync;
// 	DM_Synth.addDef;
// 	Server.default.sync;
// 	machine = DM_Synth();
// 	machine.makeMIDIInterface;
// }.fork
// )
