import { mini } from "@strudel.cycles/mini";
import { run2e } from "../output/WAGS.Run";
import { sinOsc as $sinOsc, gain as $gain } from "../output/WAGS.Control";
import { animationFrameEvent } from "../output/FRP.Event.Animate";
import * as Control_Monad_ST_Class from "../output/Control.Monad.ST.Class/index.js";
import * as FRP_Event from "../output/FRP.Event/index.js";
const create = FRP_Event.create(Control_Monad_ST_Class.monadSTEffect)(
	Control_Monad_ST_Class.monadSTEffect
);

const sinOsc = $sinOsc({ toInitializeSinOsc: (x) => x });
const gain = $gain({ toInitializeGain: (x) => x });
const miniEvent = create();
const tempoEvent = create();
