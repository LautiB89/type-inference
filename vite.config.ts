import { defineConfig } from "vite";
import { plugin as elm } from "vite-plugin-elm";

export default defineConfig({
    plugins: [elm()],
    base: "/type-inference/"
})