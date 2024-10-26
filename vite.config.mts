import { resolve } from "path";
import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";
import Sitemap from "vite-plugin-sitemap";

const sitemapConfig = {
  hostname: "https://sync-timer.netlify.app/",
};

export default defineConfig({
  plugins: [elmPlugin(), Sitemap(sitemapConfig)],
  build: {
    rollupOptions: {
      input: {
        main: resolve("index.html"),
        en: resolve("en.html"),
      },
    },
  },
  css: {
    preprocessorOptions: {
      scss: {
        api: "modern-compiler",
      },
    },
  },
});
